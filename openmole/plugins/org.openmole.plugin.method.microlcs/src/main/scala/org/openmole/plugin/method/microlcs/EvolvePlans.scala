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

import org.openmole.core.context.{ Context, Variable }
import org.openmole.core.fileservice.FileService
import org.openmole.core.workflow.builder.DefinitionScope
import org.openmole.core.workflow.dsl._
import org.openmole.core.workflow.task.ClosureTask
import org.openmole.core.workspace.NewFile
import org.openmole.tool.logger.JavaLogger
import org.openmole.tool.random.RandomProvider

/**
 * ensures we only keep a given maximum of rules
 */
object EvolvePlans extends JavaLogger {

  def apply(
    maxrules:     Int,
    microActions: Seq[MicroGenes.Gene[_]]
  )(implicit name: sourcecode.Name, definitionScope: DefinitionScope, newFile: NewFile, fileService: FileService) = {

    ClosureTask("EvolvePlans") { (context, rng, _) ⇒

      // retrieve the inputs
      // ... the current iteration
      val iteration: Int = context(varIterations)

      // ... the rules used for the exploration
      val rules: Array[ClassifierRule] = context(varRules)

      val plans: Array[MacroGene] = context(varPlans)
      val plansBefore: Array[MacroGene] = context(varPlansBefore)

      //System.out.println("Iteration " + iteration + " Here are the " + plans.length + " plans after evaluation:\n" + MacroGene.toPrettyString(plans))

      // elitism: we conserve the best of the previous as well !
      val bestPlans = HasMultiObjectivePerformance.detectParetoFront(plans ++ plansBefore)

      //System.out.println("Pareto optimal plans:\n" + MacroGene.toPrettyString(bestPlans.toList.sortWith(_.performanceAggregated(0) < _.performanceAggregated(0))))

      val plansRankedPareto = HasMultiObjectivePerformance.detectParetoFronts(plans)

      System.out.println("\n\n" + HasMultiObjectivePerformance.paretoFrontsToPrettyString(plansRankedPareto))

      // select n parents; they will be taken from the first front, then next, then next, etc...
      val parents: Iterable[MacroGene] = HasMultiObjectivePerformance.selectParentsFromFronts(maxrules, plansRankedPareto.toList)(rng)

      // we can do crossover between rules which are of similar size

      val mins: Array[Double] = context(DecodeEntities.varMin)
      val maxs: Array[Double] = context(DecodeEntities.varMax)

      // mutate !
      val plansMutated = parents.map(
        p ⇒
          if (p.rules.length > 1)
            MacroGene.mutate(
            p,
            microActions,
            mins,
            maxs,
            context
          )(rng, newFile, fileService)
          else
            p
      ).toArray

      List(
        Variable(varPlans, plansMutated),
        Variable(varPlansBefore, plans)
      )

    } set (
      // the plans are taken as inputs, and evolved before being outputed
      (inputs, outputs) += varPlans,
      // the rules we used for each entity
      (inputs, outputs) += varRules,
      (inputs, outputs) += varPlansBefore,

      (inputs, outputs) += varIterations,
      (inputs, outputs) += DecodeEntities.varEntities,
      (inputs, outputs) += DecodeEntities.varMin,
      (inputs, outputs) += DecodeEntities.varMax

    )

  }
}