/*
 * Copyright (C) 2012 reuillon
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

package org.openmole.runtime

import org.openmole.core.logconfig.LoggerConfig
import org.openmole.core.pluginmanager.PluginManager
import org.openmole.tool.file._
import org.openmole.tool.logger.JavaLogger
import scopt._
import java.io.File

import org.openmole.core.serializer.SerializerService
import org.openmole.core.communication.storage.RemoteStorage
import org.openmole.core.event.EventDispatcher
import org.openmole.core.fileservice.FileService
import org.openmole.core.preference.Preference
import org.openmole.core.threadprovider.ThreadProvider
import org.openmole.core.workspace.{ NewFile, Workspace }

object SimExplorer extends JavaLogger {

  import Log._

  def run(args: Array[String]): Int = {
    try {
      logger.finest("Running OpenMOLE runtime")

      case class Config(
        storage:       Option[String] = None,
        inputMessage:  Option[String] = None,
        outputMessage: Option[String] = None,
        path:          Option[String] = None,
        pluginPath:    Option[String] = None,
        nbThread:      Option[Int]    = None,
        workspace:     Option[String] = None,
        test:          Boolean        = false,
        debug:         Boolean        = false
      )

      val parser = new OptionParser[Config]("OpenMOLE") {
        head("OpenMOLE runtime", "0.x")
        opt[String]('s', "storage") text ("Storage") action {
          (v, c) ⇒ c.copy(storage = Some(v))
        }
        opt[String]('i', "input") text ("Path of the input message") action {
          (v, c) ⇒ c.copy(inputMessage = Some(v))
        }
        opt[String]('o', "output") text ("Path of the output message") action {
          (v, c) ⇒ c.copy(outputMessage = Some(v))
        }
        opt[String]('c', "path") text ("Path for the communication") action {
          (v, c) ⇒ c.copy(path = Some(v))
        }
        opt[String]('p', "plugin") text ("Path for plugin category to preload") action {
          (v, c) ⇒ c.copy(pluginPath = Some(v))
        }
        opt[Int]('t', "nbThread") text ("Number of thread for the execution") action {
          (v, c) ⇒ c.copy(nbThread = Some(v))
        }
        opt[String]('w', "workspace") text ("Workspace location") action {
          (v, c) ⇒ c.copy(workspace = Some(v))
        }
        opt[Unit]('d', "debug") text ("Switch on the debug mode") action {
          (_, c) ⇒ c.copy(debug = true)
        }
        opt[Unit]("test") text ("Switch on test mode") action {
          (_, c) ⇒ c.copy(test = true)
        }
      }

      parser.parse(args, Config()) foreach { config ⇒
        config.test match {
          case false ⇒

            if (config.debug) LoggerConfig.level("ALL")

            implicit val workspace = Workspace(new File(config.workspace.get).getCanonicalFile)
            implicit val newFile = NewFile(workspace)
            implicit val serializerService = SerializerService()
            implicit val preference = Preference.memory()
            implicit val threadProvider = ThreadProvider(config.nbThread.get + 5)
            implicit val fileService = FileService()
            implicit val eventDispatcher = EventDispatcher()

            try {

              PluginManager.startAll.foreach { case (b, e) ⇒ logger.log(WARNING, s"Error starting bundle $b", e) }
              logger.fine("plugins: " + config.pluginPath.get + " " + new File(config.pluginPath.get).listFilesSafe.mkString(","))
              PluginManager.tryLoad(new File(config.pluginPath.get).listFilesSafe).foreach { case (f, e) ⇒ logger.log(WARNING, s"Error loading bundle $f", e) }

              val (storage, _) = serializerService.deserialiseAndExtractFiles[RemoteStorage](new File(config.storage.get))

              new Runtime().apply(
                storage,
                config.path.get,
                config.inputMessage.get,
                config.outputMessage.get,
                config.nbThread.getOrElse(1),
                config.debug
              )
            }
            finally {
              threadProvider.stop()
            }
          case true ⇒ logger.info("The runtime is working")
        }

      }
    }
    catch {
      case t: Throwable ⇒
        logger.log(SEVERE, "Error during runtime execution", t)
        throw t
    }

    0
  }

}
