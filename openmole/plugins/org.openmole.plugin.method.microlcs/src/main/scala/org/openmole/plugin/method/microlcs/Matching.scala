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

import org.openmole.core.context.{ Context, Namespace, Variable }
import org.openmole.core.expansion.FromContext
import org.openmole.core.workflow.builder.DefinitionScope
import org.openmole.core.workflow.sampling.Sampling
import org.openmole.core.workflow.tools.ScalarOrSequenceOfDouble
import org.openmole.tool.logger.JavaLogger
import org.openmole.core.workflow.task.ClosureTask
import org.openmole.core.workflow.dsl._
import org.openmole.core.fileservice.FileService
import org.openmole.core.workspace.NewFile
import org.openmole.plugin.method.microlcs.DecodeEntities.{ varMax, varMin }
import org.openmole.tool.random.RandomProvider

object Matching extends JavaLogger {

  def covering(entity: Entity, _actions: Seq[Genes.Gene[_]], context: Context)(implicit rng: RandomProvider, newFile: NewFile, fileService: FileService): ClassifierRule = {
    ClassifierRule(entity, _actions, context)
  }

  /**
   * For a given entities, identifies the rules which are applicable and
   * returns them.
   */
  def matchOrCoverIndividual(
    rules:    Array[ClassifierRule],
    entity:   Entity,
    _actions: Seq[Genes.Gene[_]],
    context:  Context)(implicit rng: RandomProvider, newFile: NewFile, fileService: FileService): ClassifierRule = {

    val matched: Array[ClassifierRule] = rules.filter(r ⇒ r.matches(entity))
    if (matched.length == 1) {
      matched(0) // TODO
    }
    else if (matched.length > 1) {
      // select the best one ? something else ?
      //System.out.println("we might match entity " + entity + " with " + matched.length + " rules")
      //System.out.println(ClassifierRule.toPrettyString(matched.toList))
      // TODO we select randomly here... what would be the good solution?
      matched(rng().nextInt(matched.length))
    }
    else {
      System.out.println("covering entity " + entity)
      covering(entity, _actions, context)
    }
  }

  def apply(
    _actions: Seq[Genes.Gene[_]]
  )(implicit name: sourcecode.Name, definitionScope: DefinitionScope, newFile: NewFile, fileService: FileService) = {

    ClosureTask("Matching") { (context, rng, _) ⇒

      val iteration: Int = context(varIterations)
      val entities: Array[Entity] = context(DecodeEntities.varEntities)
      val rules: Array[ClassifierRule] = context(varRules)

      val rulesShuffled: Array[ClassifierRule] = rng().shuffle(rules.toList).toArray

      System.out.println(
        "Iteration " + iteration +
          " matching on " + entities.length + " entities based on " + rules.length + " rules")

      // create the set of actions to be used
      val rulesActionSet: Array[ClassifierRule] =
        entities.map { e ⇒ matchOrCoverIndividual(rulesShuffled, e, _actions, context)(rng, newFile, fileService) }
          .toArray
      //System.out.println("Here are the rules: " + ClassifierRule.toPrettyString(rulesActionSet.toList))

      // apply the rules on entities
      val entitiesUpdated: Array[Entity] =
        entities.zipWithIndex.map { case (e, i) ⇒ rulesActionSet(i).actUpon(e) }
          .toArray

      List(
        Variable(DecodeEntities.varEntities, entitiesUpdated),
        Variable(varRules, rulesActionSet)
      )

    } set (
      // we expect as inputs:
      // ... the entities to match
      inputs += DecodeEntities.varEntities,
      // ... the list of rules
      inputs += varRules,
      // .. the current iteration
      inputs += varIterations,

      // we provide as outputs
      outputs += DecodeEntities.varEntities,
      // ... the entities we decoded
      outputs += varRules,
      // ... the current iteration
      outputs += varIterations,

      (inputs, outputs) += DecodeEntities.varMin,
      (inputs, outputs) += DecodeEntities.varMax
    )

  }

}
