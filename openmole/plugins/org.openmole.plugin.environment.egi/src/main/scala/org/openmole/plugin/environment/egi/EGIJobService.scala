package org.openmole.plugin.environment.egi

import gridscale.dirac.JobDescription
import org.openmole.core.exception.InternalProcessingError
import org.openmole.plugin.environment.batch.environment.{ AccessControl, BatchEnvironment, SerializedJob }
import org.openmole.plugin.environment.batch.storage.EnvironmentStorage
import org.openmole.tool.cache.TimeCache

object EGIJobService {

  def apply(diracService: _root_.gridscale.dirac.DIRACServer, environment: EGIEnvironment[_]) =
    new EGIJobService(diracService, environment)

}

class EGIJobService(diracService: _root_.gridscale.dirac.DIRACServer, environment: EGIEnvironment[_]) {

  import environment._
  import interpreters._
  import services._

  lazy val diracJobGroup = java.util.UUID.randomUUID().toString.filter(_ != '-')

  def submit(serializedJob: SerializedJob, outputPath: String, storageLocation: String) = {
    import org.openmole.tool.file._

    newFile.withTmpFile("script", ".sh") { script ⇒
      script.content = JobScript.create(
        serializedJob,
        outputPath,
        storageLocation,
        voName = voName,
        memory = BatchEnvironment.openMOLEMemoryValue(openMOLEMemory).toMegabytes.toInt,
        threads = 1,
        debug = debug
      )

      val jobDescription =
        JobDescription(
          executable = "/bin/bash",
          arguments = s"-x ${script.getName}",
          inputSandbox = Seq(script),
          stdOut = Some(EGIEnvironment.stdOutFileName),
          stdErr = Some(EGIEnvironment.stdErrFileName),
          outputSandbox = Seq(EGIEnvironment.stdOutFileName, EGIEnvironment.stdErrFileName),
          cpuTime = cpuTime
        )

      accessControl { gridscale.dirac.submit(diracService, jobDescription, tokenCache(), Some(diracJobGroup)) }
    }
  }

  lazy val jobStateCache = TimeCache { () ⇒
    val states = accessControl { gridscale.dirac.queryState(diracService, tokenCache(), groupId = Some(diracJobGroup)) }
    states.toMap -> preference(EGIEnvironment.JobGroupRefreshInterval)
  }

  def state(id: gridscale.dirac.JobID) = {
    val state = jobStateCache().getOrElse(id.id, throw new InternalProcessingError(s"Job ${id.id} not found in group ${diracJobGroup} of DIRAC server."))
    org.openmole.plugin.environment.gridscale.GridScaleJobService.translateStatus(state)
  }

  def delete(id: gridscale.dirac.JobID) = {
    accessControl { gridscale.dirac.delete(diracService, tokenCache(), id) }
  }

  def stdOutErr(id: gridscale.dirac.JobID) = {
    newFile.withTmpDir { tmpDir ⇒
      import org.openmole.tool.file._
      tmpDir.mkdirs()
      accessControl { gridscale.dirac.downloadOutputSandbox(diracService, tokenCache(), id, tmpDir) }

      def stdOut =
        if ((tmpDir / EGIEnvironment.stdOutFileName) exists) (tmpDir / EGIEnvironment.stdOutFileName).content
        else ""

      def stdErr =
        if ((tmpDir / EGIEnvironment.stdErrFileName) exists) (tmpDir / EGIEnvironment.stdErrFileName).content
        else ""

      (stdOut, stdErr)
    }
  }

  lazy val accessControl = AccessControl(preference(EGIEnvironment.ConnectionsToDIRAC))
}
