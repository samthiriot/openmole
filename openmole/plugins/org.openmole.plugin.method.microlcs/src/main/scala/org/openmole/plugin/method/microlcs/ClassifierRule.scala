/*
 * Copyright (C) 2018 Samuel Thiriot
 *                    Romain Reuillon
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
import org.openmole.core.workspace.NewFile
import org.openmole.tool.random.RandomProvider

abstract class AbstractClassifier {

  val name: String
  val age: Int

  val conditions: Array[Condition[_]]
  val actions: Array[Variable[Q] forSome { type Q }]

  var performance: Seq[Seq[Double]]
  var history: List[String] = List()

  def applications(): Int = {
    if (performance.isEmpty) { 0 }
    else { performance(0).length }
  }

  override def toString: String =
    name + ": \t" +
      "if " + conditions.map(c ⇒ c.toString).mkString(" and ") +
      " \tthen set " + actions.map(a ⇒ a.toString).mkString(", ") +
      " \t " + (if (performance.isEmpty) "(0)" else "(" + performance(0).length + ") [" + performanceAggregated().map(v ⇒ v.toString).mkString(",") + "]")

  def dominatesPareto(other: AbstractClassifier) = {
    val perfMine = performanceAggregated()
    val perfOther = other.performanceAggregated()
    val zipped = perfMine zip perfOther
    zipped.forall { case (m, o) ⇒ m <= o } && zipped.exists { case (m, o) ⇒ m < o }
  }

  def subsums(other: AbstractClassifier) = conditions.zipWithIndex.forall { case (c, i) ⇒ c.subsumsUnsafe(other.conditions(i)) }

  def sameActions(other: AbstractClassifier) = actions.zipWithIndex.forall { case (a, i) ⇒ a.value == other.actions(i).value }
  def similarPerformance(other: AbstractClassifier, epsilons: Array[Double]) = (performanceAggregated() zip other.performanceAggregated() zip epsilons).forall { case ((p1, p2), epsilon) ⇒ Math.abs(p1 - p2) < epsilon }

  def sameConditions(other: AbstractClassifier) = conditions.zipWithIndex.forall { case (c, i) ⇒ c.equals(other.conditions(i)) }

  def performanceAggregated() = performance.map(vals ⇒ vals.sum / vals.length)

  def performanceAggregated(i: Int) = performance(i).sum / performance(i).length

  /**
   * Returns true if the conditions of the classifier are verified by the entity passed as a parameter
   */
  def matches(entity: Entity): Boolean = conditions.zipWithIndex.forall { case (c, i) ⇒ c.acceptsUnsafe(entity.characteristics(i)) } // matches .value

  /**
   * Applies the action of the classifier over this entity.
   * Returns a copy of this entity with different values for actions
   */
  def actUpon(entity: Entity): Entity = entity.copy(actions = actions.map(av ⇒ av).toArray) // TODO clone ???

  def absorb(other: ClassifierRule) = {

    // update history
    //history = history ::: List("absorbed " + other)

    // integrate the performance of the other
    performance = performance.zipWithIndex.map { case (p, i) ⇒ p ++ other.performance(i) }

  }

  def addPerformance(exp: Seq[Double]) = {
    if (performance.isEmpty) {
      // we had no perf; let's create it
      performance = exp.map(v ⇒ List(v))
    }
    else {
      // we have perf: let's just update it
      performance = performance.zipWithIndex
        .map { case (l, i) ⇒ l :+ exp(i) }
    }
  }

}

case class ClassifierRule(
  name:            String,
  conditions:      Array[Condition[_]],
  actions:         Array[Variable[Q] forSome { type Q }],
  age:             Int,
  var performance: Seq[Seq[Double]]
) extends AbstractClassifier

object ClassifierRule {

  var lastId: Int = 0

  def getNextName(): String = {
    ClassifierRule.lastId = ClassifierRule.lastId + 1
    Integer.toString(ClassifierRule.lastId, 36).toUpperCase
  }

  /**
   * Generates a random rule matching this entity
   */
  def apply(
    e:        Entity,
    _actions: Seq[MicroGenes.Gene[_]],
    context:  Context)(implicit rng: RandomProvider, newFile: NewFile, fileService: FileService): ClassifierRule = {

    ClassifierRule(
      // generate an alphanumeric string
      getNextName(),
      // generate random conditions matching entity characteristics
      e.characteristics.map(c ⇒ Condition(c, rng())).toArray,
      // micro actions are random
      _actions.map(a ⇒ Variable.unsecure(a.prototype, a.makeRandomValue(context))).toArray, // TODO
      0,
      Seq()
    )
  }

  def toPrettyString(rules: List[ClassifierRule]) = rules.map(r ⇒ r.toString).mkString("\n")

  def crossoverSinglePoint(a: ClassifierRule, b: ClassifierRule)(implicit rng: RandomProvider): (ClassifierRule, ClassifierRule) = {

    val rand = rng()

    val cutoffCondition: Int = rand.nextInt(a.conditions.length)
    val cutoffActions: Int = rand.nextInt(a.actions.length)

    (
      a.copy(
        name = getNextName(),
        conditions = a.conditions.slice(0, cutoffCondition) ++ b.conditions.slice(cutoffCondition, b.conditions.length),
        actions = a.actions.slice(0, cutoffActions) ++ b.actions.slice(cutoffActions, b.conditions.length),
        performance = Seq()
      ),
        b.copy(
          name = getNextName(),
          conditions = b.conditions.slice(0, cutoffCondition) ++ a.conditions.slice(cutoffCondition, b.conditions.length),
          actions = b.actions.slice(0, cutoffActions) ++ a.actions.slice(cutoffActions, b.conditions.length),
          performance = Seq()
        )
    )
  }

  def mutate(r: ClassifierRule, microActions: Seq[MicroGenes.Gene[_]], mins: Array[Double], maxs: Array[Double], context: Context)(implicit rng: RandomProvider, newFile: NewFile, fileService: FileService): ClassifierRule = {
    val rand = rng()
    if (rand.nextDouble() <= r.conditions.length.toDouble / (r.conditions.length + r.actions.length)) {
      // change conditions
      val idxChange = rand.nextInt(r.conditions.length)
      r.copy(
        name = getNextName(),
        conditions = r.conditions.slice(0, idxChange - 1) ++
          Array(Condition.mutate(r.conditions(idxChange), mins(idxChange), maxs(idxChange))) ++
          r.conditions.slice(idxChange + 1, r.conditions.length),
        performance = Seq()
      )
    }
    else {
      // change actions
      val idxChange = rand.nextInt(r.actions.length)
      r.copy(
        name = getNextName(),
        actions =
          r.actions.slice(0, idxChange - 1) ++
            Array(
              Variable.unsecure(
                microActions(idxChange).prototype,
                microActions(idxChange).makeRandomValue(context)
              )) ++
              r.actions.slice(idxChange + 1, r.conditions.length),
        performance = Seq()
      )
    }

  }

}

