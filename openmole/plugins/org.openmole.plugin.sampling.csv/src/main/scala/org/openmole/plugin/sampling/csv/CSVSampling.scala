/*
 * Copyright (C) 2011 mathieu
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

package org.openmole.plugin.sampling.csv

import java.io.File

import monocle.Lens
import monocle.macros.Lenses
import org.openmole.core.context.Val
import org.openmole.core.expansion.FromContext
import org.openmole.core.workflow.builder.{ InputOutputBuilder, InputOutputConfig, Mapped, MappedInputBuilder }
import org.openmole.core.workflow.sampling._
import org.openmole.core.workflow.tools._
import org.openmole.plugin.tool.csv._

object CSVSampling {

  implicit def isIO = InputOutputBuilder(CSVSampling.config)

  implicit def isBuilder: CSVToVariablesBuilder[CSVSampling] = new CSVToVariablesBuilder[CSVSampling] {
    override def mappedOutputs: Lens[CSVSampling, Vector[Mapped[_]]] = CSVSampling.columns
    override def fileColumns = CSVSampling.fileColumns
    override def separator = CSVSampling.separator
  }

  def apply(file: FromContext[File], separator: OptionalArgument[Char] = None): CSVSampling =
    new CSVSampling(
      file,
      config = InputOutputConfig(),
      columns = Vector.empty,
      fileColumns = Vector.empty,
      separator = separator.option
    )

  def apply(file: File): CSVSampling = apply(file: FromContext[File])
  def apply(directory: File, name: FromContext[String]): CSVSampling = apply(FileList(directory, name))

}

@Lenses case class CSVSampling(
  file:        FromContext[File],
  config:      InputOutputConfig,
  columns:     Vector[Mapped[_]],
  fileColumns: Vector[(String, File, Val[File])],
  separator:   Option[Char]
) extends Sampling {

  override def inputs = InputOutputConfig.inputs.get(config)
  override def prototypes = InputOutputConfig.outputs.get(config)
  override def apply() = FromContext { p ⇒
    import p._
    csvToVariables(
      columns,
      fileColumns,
      separator)(file.from(context), context)
  }

}
