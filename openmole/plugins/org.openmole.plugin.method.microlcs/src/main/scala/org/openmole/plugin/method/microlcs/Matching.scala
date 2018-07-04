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

object Matching extends JavaLogger {

  def covering(entity: Entity, rng: scala.util.Random): ClassifierRule = {
    ClassifierRule(entity, rng)
  }

  /**
   * For a given entities, identifies the rules which are applicable and
   * returns them.
   */
  def matchOrCoverIndividual(rules: Array[ClassifierRule], entity: Entity, rng: scala.util.Random): ClassifierRule = {
    val matched: Array[ClassifierRule] = rules.filter(r ⇒ r.matches(entity))
    if (matched.length == 1) {
      matched(1) // TODO
    }
    else if (matched.length > 1) {
      // select the best one ? something else ?
      matched(1) // TODO
    }
    else {
      covering(entity, rng)
    }
  }

  def apply()(implicit name: sourcecode.Name, definitionScope: DefinitionScope) = {

    ClosureTask("Matching") { (context, rng, _) ⇒

      val random = rng()

      val entities: Array[Entity] = context(DecodeEntities.varEntities)
      val rules: Array[ClassifierRule] = context(varRules)

      System.out.println("matching on " + entities.length + " entities based on " + rules.length + " rules")

      val rulesActionSet: Array[ClassifierRule] = entities.map(e ⇒ matchOrCoverIndividual(rules, e, random)).toArray

      System.out.println("Here are the rules: " + rulesActionSet)

      List(
        Variable(DecodeEntities.varEntities, entities),
        Variable(varRules, rulesActionSet)
      )

    } set (
      // we expect as inputs:
      // ... the entities to match
      inputs += DecodeEntities.varEntities,
      // ... the list of rules
      inputs += varRules,
      // we provide as outputs
      outputs += DecodeEntities.varEntities,
      // ... the entities we decoded
      outputs += varRules
    )

  }

}
