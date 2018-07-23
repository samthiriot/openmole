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
import org.openmole.core.workspace.NewFile
import org.openmole.tool.random.RandomProvider

abstract class AbstractClassifier extends HasMultiObjectivePerformance {

  val id: Int
  val name: String
  val age: Int

  val conditions: Array[Condition[_]]
  val actions: Array[Variable[Q] forSome { type Q }]

  override def toString: String =
    name + ": \t" +
      "if " + conditions.map(c ⇒ c.toString).mkString(" and ") +
      " \tthen set " + actions.map(a ⇒ a.toString).mkString(", ") +
      " \t " + performanceToString

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

  /**
   * Returns true if the conditions of the classifier are verified by the entity passed as a parameter
   */
  def matches(entity: Entity): Boolean = conditions.zipWithIndex.forall { case (c, i) ⇒ c.acceptsUnsafe(entity.characteristics(i)) } // matches .value

  /**
   * Applies the action of the classifier over this entity.
   * Returns a copy of this entity with different values for actions
   */
  def actUpon(entity: Entity): Entity = entity.copy(actions = actions.map(av ⇒ av).toArray) // TODO clone ???

  /**
   * Returns an integer index of generality; the higher the more general.
   *
   */
  def generalityIndice(): Int = conditions.map(_.generalityIndice).sum

  def distanceAction(a1: Any, a2: Any): Double = (a1, a2) match {
    case (i1: Integer, i2: Integer) ⇒ Math.abs(i2 - i1)
    case (d1: Double, d2: Double)   ⇒ Math.abs(d2 - d1)
    case (b1: Boolean, b2: Boolean) ⇒ if (b1 == b2) 0.0 else 1.0
    case (s1: String, s2: String)   ⇒ if (s1 == s2) 0.0 else 1.0
    case (o1: Any, o2: Any)         ⇒ if (o1.equals(o2)) 0.0 else 1.0
    case _                          ⇒ throw new IllegalArgumentException("cannot compute the distance between two actions " + a1 + " and " + a2)
  }

  def distanceActions(other: ClassifierRule): Double = (actions zip other.actions).map { case (a1, a2) ⇒ distanceAction(a1.value, a2.value) }.sum
}

case class ClassifierRule(
  id:                       Int,
  name:                     String,
  conditions:               Array[Condition[_]],
  actions:                  Array[Variable[Q] forSome { type Q }],
  age:                      Int,
  override var performance: Seq[Seq[Double]]
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
      ClassifierRule.lastId,
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

  def mutateConditions(r: ClassifierRule, mins: Array[Double], maxs: Array[Double], context: Context)(implicit rng: RandomProvider, newFile: NewFile, fileService: FileService): ClassifierRule = {
    val rand = rng()
    val idxChange = rand.nextInt(r.conditions.length)
    // debug : System.out.println("mutating condition " + idxChange + " in " + r)
    val res = r.copy(
      name = getNextName(),
      conditions = r.conditions.slice(0, idxChange) ++
        Array(Condition.mutate(r.conditions(idxChange), mins(idxChange), maxs(idxChange))) ++
        r.conditions.slice(idxChange + 1, r.conditions.length),
      performance = Seq()
    )
    System.out.println("=>  " + res)
    res
  }

  def mutateActions(r: ClassifierRule, microActions: Seq[MicroGenes.Gene[_]], mins: Array[Double], maxs: Array[Double], context: Context)(implicit rng: RandomProvider, newFile: NewFile, fileService: FileService): ClassifierRule = {
    // change actions
    val rand = rng()
    val idxChange = rand.nextInt(r.actions.length)
    r.copy(
      name = getNextName(),
      actions =
        r.actions.slice(0, idxChange) ++
          Array(
            Variable.unsecure(
              microActions(idxChange).prototype,
              microActions(idxChange).makeRandomValue(context)
            )) ++
            r.actions.slice(idxChange + 1, r.conditions.length),
      performance = Seq()
    )
  }

  def mutate(r: ClassifierRule, microActions: Seq[MicroGenes.Gene[_]], mins: Array[Double], maxs: Array[Double], context: Context)(implicit rng: RandomProvider, newFile: NewFile, fileService: FileService): ClassifierRule = {
    val rand = rng()
    if (rand.nextDouble() <= r.conditions.length.toDouble / (r.conditions.length + r.actions.length)) {
      mutateConditions(r, mins, maxs, context)
    }
    else {
      mutateActions(r, microActions, mins, maxs, context)
    }
  }

}

