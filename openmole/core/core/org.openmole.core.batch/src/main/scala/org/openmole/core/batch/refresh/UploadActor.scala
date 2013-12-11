/*
 * Copyright (C) 2012 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.core.batch.refresh

import akka.actor.Actor

import akka.actor.ActorRef
import com.db4o.ObjectContainer
import com.ice.tar.TarOutputStream
import java.io.File
import java.util.UUID
import org.openmole.core.batch.message._
import org.openmole.core.batch.replication._
import org.openmole.core.batch.storage._
import org.openmole.core.batch.control._
import org.openmole.core.batch.environment._
import org.openmole.core.batch.environment.BatchEnvironment.{ signalUpload }
import org.openmole.core.model.job._
import org.openmole.misc.tools.io.FileUtil._
import org.openmole.misc.tools.io.TarArchiver._

import org.openmole.core.serializer._
import org.openmole.misc.fileservice.FileService
import org.openmole.misc.hashservice.HashService
import org.openmole.misc.workspace.Workspace
import scala.collection.immutable.TreeSet
import org.openmole.misc.exception.UserBadDataError

class UploadActor(jobManager: ActorRef) extends Actor {

  def receive = {
    case msg @ Upload(job, storage) ⇒
      if (!job.state.isFinal) {
        try {
          initCommunication(job.environment, job.job, storage) match {
            case Some(sj) ⇒ jobManager ! Uploaded(job, sj)
            case None     ⇒ jobManager ! Delay(msg, Workspace.preferenceAsDuration(BatchEnvironment.NoTokenForSerivceRetryInterval).toMilliSeconds)
          }
        }
        catch {
          case e: Throwable ⇒ signalError(job, e, msg)
        }
      }
      System.runFinalization()
  }

  private def signalError(job: BatchExecutionJob, e: Throwable, msg: Upload) = {
    jobManager ! Error(job, e)
    jobManager ! msg
  }

  private def initCommunication(environment: BatchEnvironment, job: IJob, selectedStorage: Option[StorageService]): Option[SerializedJob] = Workspace.withTmpFile("job", ".tar") { jobFile ⇒

    val (serializationFile, serialisationPluginFiles) = serializeJob(jobFile, job)

    val storageAndToken = selectedStorage match {
      case None ⇒
        val selected = environment.selectAStorage(
          (serializationFile +
            environment.runtime +
            environment.jvmLinuxI386 +
            environment.jvmLinuxX64 ++
            environment.plugins ++
            serialisationPluginFiles).map(f ⇒ f -> FileService.hash(job.moleExecution, f)))
        Some(selected)
      case Some(storage) ⇒ storage.tryGetToken.map(storage -> _)
    }

    storageAndToken.map {
      case (storage, token) ⇒

        implicit val t = token
        try ReplicaCatalog.withClient { implicit client ⇒
          val communicationPath = storage.child(storage.tmpDir, UUID.randomUUID.toString)
          storage.makeDir(communicationPath)

          val inputPath = storage.child(communicationPath, Storage.uniqName("job", ".in"))

          val runtime = replicateTheRuntime(job, environment, storage)

          val executionMessage = createExecutionMessage(
            job,
            jobFile,
            serializationFile,
            serialisationPluginFiles,
            storage,
            communicationPath)

          /* ---- upload the execution message ----*/
          Workspace.withTmpFile("job", ".xml") { executionMessageFile ⇒
            SerialiserService.serialise(executionMessage, executionMessageFile)
            signalUpload(storage.uploadGZ(executionMessageFile, inputPath), inputPath, storage)
          }

          SerializedJob(storage, communicationPath, inputPath, runtime)
        } finally storage.releaseToken(token)
    }
  }

  def serializeJob(file: File, job: IJob) = {
    var files = new TreeSet[File]
    var plugins = new TreeSet[File]

    val tos = new TarOutputStream(file.bufferedOutputStream)
    try {
      for (moleJob ← job.moleJobs) moleJob.synchronized {
        if (!moleJob.finished) {
          val moleJobFile = Workspace.newFile("job", ".tar")
          try {
            val serializationResult =
              SerialiserService.serialiseGetPluginsAndFiles(
                RunnableTask(moleJob),
                moleJobFile)

            files ++= serializationResult.files
            plugins ++= serializationResult.plugins

            tos.addFile(moleJobFile, UUID.randomUUID.toString)
          }
          finally moleJobFile.delete
        }
      }
    }
    finally tos.close
    (files, plugins)
  }

  def toReplicatedFile(job: IJob, file: File, storage: StorageService)(implicit token: AccessToken, objectContainer: ObjectContainer): ReplicatedFile = {
    if (!file.exists) throw new UserBadDataError(s"File/category $file is requiered but doesn't exist.")

    val isDir = file.isDirectory
    var toReplicate = file
    val toReplicatePath = file.getAbsoluteFile

    //Hold cache to avoid gc and file deletion
    val cache = if (isDir) {
      val cache = FileService.archiveForDir(job.moleExecution, file)
      toReplicate = cache.file(false)
      cache
    }
    else null

    val hash = FileService.hash(job.moleExecution, toReplicate).toString
    val replica = ReplicaCatalog.uploadAndGet(toReplicate, toReplicatePath, hash, storage)
    ReplicatedFile(file, isDir, hash, replica.path, file.mode)
  }

  def replicateTheRuntime(
    job: IJob,
    environment: BatchEnvironment,
    storage: StorageService)(implicit token: AccessToken, objectContainer: ObjectContainer) = {

    val environmentPluginPath = environment.plugins.map { p ⇒ toReplicatedFile(job, p, storage) }.map { FileMessage(_) }
    val runtimeFileMessage = FileMessage(toReplicatedFile(job, environment.runtime, storage))
    val jvmLinuxI386FileMessage = FileMessage(toReplicatedFile(job, environment.jvmLinuxI386, storage))
    val jvmLinuxX64FileMessage = FileMessage(toReplicatedFile(job, environment.jvmLinuxX64, storage))

    val storageReplication = FileMessage(toReplicatedFile(job, storage.serializedRemoteStorage, storage))

    Runtime(
      storageReplication,
      runtimeFileMessage,
      environmentPluginPath,
      jvmLinuxI386FileMessage,
      jvmLinuxX64FileMessage)
  }

  def createExecutionMessage(
    job: IJob,
    jobFile: File,
    serializationFile: Iterable[File],
    serializationPlugin: Iterable[File],
    storage: StorageService,
    path: String)(implicit token: AccessToken, objectContainer: ObjectContainer): ExecutionMessage = {
    val jobForRuntimePath = storage.child(path, Storage.uniqName("job", ".tgz"))

    signalUpload(
      storage.uploadGZ(jobFile, jobForRuntimePath), jobForRuntimePath, storage)
    val jobHash = HashService.computeHash(jobFile).toString

    val pluginReplicas = serializationPlugin.map { toReplicatedFile(job, _, storage) }
    val files = serializationFile.map { toReplicatedFile(job, _, storage) }

    ExecutionMessage(
      pluginReplicas,
      files,
      FileMessage(jobForRuntimePath, jobHash),
      path)
  }

}
