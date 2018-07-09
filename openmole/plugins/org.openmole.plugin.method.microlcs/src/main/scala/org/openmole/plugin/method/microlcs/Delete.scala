/*
 * Copyright (C) 2018 Samuel Thiriot
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.plugin.method.microlcs

import org.openmole.core.context.{ Context, Namespace, Val, Variable }
import org.openmole.core.expansion.FromContext
import org.openmole.core.workflow.builder.DefinitionScope
import org.openmole.core.workflow.sampling.Sampling
import org.openmole.core.workflow.tools.ScalarOrSequenceOfDouble
import org.openmole.tool.logger.JavaLogger
import org.openmole.core.workflow.task.ClosureTask
import org.openmole.core.workflow.dsl._
import org.openmole.core.fileservice.FileService
import org.openmole.core.workspace.NewFile
import org.openmole.tool.random.RandomProvider

/**
 * ensures we only keep a given maximum of rules
 */
object Delete extends JavaLogger {

  def apply(
    maxrules: Int
  )(implicit name: sourcecode.Name, definitionScope: DefinitionScope, newFile: NewFile, fileService: FileService) = {

    ClosureTask("Evaluate") { (context, rng, _) ⇒

      // retrieve the inputs
      // ... the current iteration
      val iteration: Int = context(varIterations)

      // ... the rules used for the exploration
      val rules: Array[ClassifierRule] = context(varRules)

      System.out.println("there are " + rules.length + "rules, we can only keep a max of " + maxrules)
      System.out.println("(nota: deletion to be developed !)")
      // TODO

      List(
        Variable(varRules, rules)
      )

    } set (
      // we expect as inputs:
      // ... the rules we used for each entity
      inputs += varRules,

      // we provide as outputs
      //outputs += DecodeEntities.varEntities,
      // ... the rules we updates with the novel information
      outputs += varRules,

      (inputs, outputs) += varIterations,
      (inputs, outputs) += DecodeEntities.varEntities,
      (inputs, outputs) += DecodeEntities.varMin,
      (inputs, outputs) += DecodeEntities.varMax

    )

  }
}