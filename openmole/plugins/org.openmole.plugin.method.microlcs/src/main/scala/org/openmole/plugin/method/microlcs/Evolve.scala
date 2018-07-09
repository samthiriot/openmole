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

import org.openmole.core.context.{ Context, Namespace, Variable, Val }
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
 * Takes a set of rules, and evolves them
 * to reach another set of rules
 */
object Evolve extends JavaLogger {

  def selectBest(indices: List[Int], rules: Array[ClassifierRule], lastSoFar: ClassifierRule, bestSoFar: ClassifierRule): (ClassifierRule, ClassifierRule) = indices match {
    case Nil ⇒
      //System.out.println("returning " + bestSoFar + " " + lastSoFar)
      (bestSoFar, lastSoFar)
    case idx :: indicesTail ⇒
      val candidate = rules(idx)
      //System.out.println("who's the best?\n\tcandidate: " + candidate + "\n\tbest so far: " + bestSoFar + "\n\tlast so far: " + lastSoFar)
      if (bestSoFar == null || lastSoFar == null || candidate.dominatesPareto(bestSoFar)) {
        selectBest(indicesTail, rules, bestSoFar, candidate)
      }
      else {
        selectBest(indicesTail, rules, lastSoFar, bestSoFar)
      }
  }

  /**
   * Deterministic tournament selection
   */
  def tournamentSelectionWithN(count: Int, rules: Array[ClassifierRule])(implicit rng: RandomProvider): (ClassifierRule, ClassifierRule) =
    selectBest(
      // the list of indices is random
      rng().shuffle((0 until rules.length).toList).take(count),
      // the rules
      rules,
      null,
      null
    )

  def apply(
    microActions: Seq[Genes.Gene[_]],
    rulesCount:   Int
  )(implicit name: sourcecode.Name, definitionScope: DefinitionScope, newFile: NewFile, fileService: FileService) = {

    ClosureTask("Evolve") { (context, rng, _) ⇒

      // retrieve the inputs
      // ... the rules used for the exploration
      val rules: Array[ClassifierRule] = context(varRules)
      val iteration: Int = context(varIterations)

      // TODO val tournamentSize =

      System.out.println("Iteration "+iteration+": starting evolution of the "+rules.length+" rules...")

      val rulesUpdated =
        (1 to rules.length / 2).map( _ => tournamentSelectionWithN(4, rules)(rng) )
                      .map{ case (a: ClassifierRule, b: ClassifierRule) => ClassifierRule.crossoverSinglePoint(a, b)(rng) }
                      .flatMap{ case (c: ClassifierRule, d: ClassifierRule) => List(
                          ClassifierRule.mutate(c, microActions, context)(rng, newFile, fileService),
                          ClassifierRule.mutate(d, microActions, context)(rng, newFile, fileService)) }
                      .toArray

      /*var x: Int = 0
      for (x ← 1 to 20) {
        System.out.println("iteration " + x)
        val (a: ClassifierRule, b: ClassifierRule) = tournamentSelectionWithN(4, rules)(rng)
        System.out.println("Selected:\n\t" + a + "\n\t" + b)

        val (c: ClassifierRule, d: ClassifierRule) = ClassifierRule.crossoverSinglePoint(a, b)(rng)
        System.out.println("After crossover:\n\t" + c + "\n\t" + d)

        val e = ClassifierRule.mutate(c, microActions, context)(rng, newFile, fileService)
        val f = ClassifierRule.mutate(d, microActions, context)(rng, newFile, fileService)

        System.out.println("After mutation:\n\t" + e + "\n\t" + f)

      }
    */

      System.out.println("Rules after evolution:\n" + ClassifierRule.toPrettyString(rulesUpdated.toList))
      List(
        Variable(varRules, rules ++ rulesUpdated)
      )

    } set (
      // we expect as inputs:
      // ... the rules we used last time
      inputs += varRules,

      // we provide as outputs
      // ... the rules we updates with the novel information
      outputs += varRules,

      (inputs, outputs) += varIterations,
      (inputs, outputs) += DecodeEntities.varEntities,

    )

  }
}