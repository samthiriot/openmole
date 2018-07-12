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

import org.openmole.core.context.{ Val, Variable }
import org.openmole.core.fileservice.FileService
import org.openmole.core.workflow.builder.DefinitionScope
import org.openmole.core.workflow.dsl._
import org.openmole.core.workflow.task.ClosureTask
import org.openmole.core.workspace.NewFile
import org.openmole.tool.logger.JavaLogger

/**
 * Takes rules and entities,
 * and elaborates the initial plans to explore
 */
object GenerateInitPlans extends JavaLogger {

  def takeNRules(n: Int, rules: Array[ClassifierRule]): Array[ClassifierRule] = {
    if (rules.length <= n) {
      rules
    }
    else {
      val sk = rules.length / n
      rules.zipWithIndex
        .filter { case (c, i) ⇒ (i % n == sk) }
        .map { case (c, i) ⇒ c }
    }
  }

  def sortByNthMicroPerf(i: Int, rules: Array[ClassifierRule]): Array[ClassifierRule] =
    rules.sortWith(
      (r1, r2) ⇒ r1.performanceAggregated(i) < r2.performanceAggregated(i)
    )

  def elaboratePlan(
    id:                Int,
    rulesSelected:     Array[ClassifierRule],
    rulesAll:          Array[ClassifierRule],
    entitiesToProcess: List[Entity]): MacroGene = entitiesToProcess match {

    case Nil ⇒ // end of process
      MacroGene(id, rulesSelected)

    case e :: entities ⇒
      val rulesMatching = rulesAll.filter(r ⇒ r.matches(e))
      if (rulesMatching.isEmpty) {
        throw new RuntimeException("unable to find a rule to match entity " + e + ", this is not theoretically possible!")
      }

      val ruleToUse = if (rulesMatching.length == 1) {
        // only one possibility, let's use it ^^
        rulesMatching(0)
      }
      else {
        // several rules match this entity

        /*
        rulesMatching.foreach(
          r ⇒ System.out.println(
            "rule " + r + " => distance actions: " + rulesSelected.map(r.distanceActions(_)).sum + " \t generality :" + r.generalityIndice()
          )
        )*/

        // we order them according to their difference of actions and also decreasing generality
        rulesMatching.sortWith(
          (r1, r2) ⇒ r1.generalityIndice() > r2.generalityIndice()
        ).sortWith(
            (r1, r2) ⇒ rulesSelected.map(r1.distanceActions(_)).sum > rulesSelected.map(r2.distanceActions(_)).sum
          )(0)

      }

      // recusrively continue to build the plan
      elaboratePlan(
        id,
        rulesSelected ++ Array(ruleToUse),
        rulesAll,
        entitiesToProcess.filterNot(ruleToUse.matches(_))
      )

  }

  /**
   * Takes a given rule, and builds a
   */
  def elaboratePlanAroundARule(
    id:       Int,
    rule:     ClassifierRule,
    rules:    Array[ClassifierRule],
    entities: Array[Entity]
  ): MacroGene = elaboratePlan(
    id,
    Array(rule),
    rules,
    entities.filter(e ⇒ !rule.matches(e)).toList
  )

  def apply(
    microMinimize: Seq[Val[Double]],
    microMaximize: Seq[Val[Double]],
    maxrules:      Int
  )(implicit name: sourcecode.Name, definitionScope: DefinitionScope, newFile: NewFile, fileService: FileService) = {

    ClosureTask("GenerateInitPlans") { (context, rng, _) ⇒

      // retrieve the inputs
      // ... the current iteration
      val iteration: Int = context(varIterations)

      // ... the rules used for the exploration
      val rules: Array[ClassifierRule] = context(varRules)
      val entities: Array[Entity] = context(DecodeEntities.varEntities)

      System.out.println("Generating the " + maxrules + " initial plans to be explored")

      val rulesFiltered: Array[ClassifierRule] = rules.filter(r ⇒ (r.applications() > 0))

      val countPerMicro = maxrules / (microMinimize.length + microMaximize.length)

      val plans: Array[MacroGene] =
        (microMaximize ++ microMaximize)
          .zipWithIndex
          .flatMap {
            case (t, i) ⇒ takeNRules(
              countPerMicro,
              sortByNthMicroPerf(i, rulesFiltered)
            ).zipWithIndex
              .map {
                case (r, j) ⇒ elaboratePlanAroundARule(i * (countPerMicro + 1) + j, r, rulesFiltered.reverse, entities)
              }
          }.toArray

      // TODO eliminate doubles

      System.out.println("Here are the initial plans to be explored:\n" + MacroGene.toPrettyString(plans))

      List(
        Variable(varRules, rulesFiltered),
        Variable(varPlans, plans),
        Variable(varIterations, iteration + 1)
      )

    } set (
      // we expect as inputs:
      // ... the rules we used for each entity
      inputs += varRules,

      // we provide as outputs
      //outputs += DecodeEntities.varEntities,
      // ... the rules we updates with the novel information
      (inputs, outputs) += varIterations,
      (inputs, outputs) += DecodeEntities.varEntities,
      outputs += varPlans,
      outputs += varRules,

      (inputs, outputs) += DecodeEntities.varMin,
      (inputs, outputs) += DecodeEntities.varMax

    /*
      rIterations,
    DecodeEntities.varEntities,
    varPlans,
    varRules,
    DecodeEntities.varMin, DecodeEntities.varMax
       */
    )

  }
}