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

package org.openmole.plugin.method

import org.openmole.core.context.{ Context, Val, _ }
import org.openmole.core.expansion.FromContext
import org.openmole.core.outputmanager.OutputManager
import org.openmole.core.workflow.builder.DefinitionScope
import org.openmole.core.workflow.dsl._
import org.openmole.core.workflow.mole._
import org.openmole.core.workflow.puzzle._
import org.openmole.core.workflow.sampling._
import org.openmole.core.workflow.task._
import org.openmole.core.workflow.tools.ScalarOrSequenceOfDouble
import org.openmole.core.workflow.validation.DataflowProblem._
import org.openmole.core.workflow.validation._
import org.openmole.core.workflow.transition.Slot
import org.openmole.tool.random.RandomProvider
import org.openmole.core.workspace.NewFile
import org.openmole.core.fileservice.FileService

import Numeric.Implicits._
import Ordering.Implicits._
import scala.reflect.ClassTag

package object microlcs {

  val namespaceMicroLCS = Namespace("microlcs")

  // this value will contain the set of rules
  val varRules = Val[Array[ClassifierRule]]("rules", namespace = namespaceMicroLCS)

  implicit def scope = DefinitionScope.Internal

  trait Condition[T] {
    def attributeName: String
    def matches(v: T): Boolean
    def accepts(v: Variable[T]): Boolean = (v.prototype.simpleName == attributeName) && matches(v.value)
    def acceptsUnsafe(v: Variable[_]) = v match {
      case vcasted: Variable[T] ⇒ accepts(vcasted)
      case _                    ⇒ throw new IllegalArgumentException("expecting another type for " + this)
    }

  }

  abstract class ConditionOneValue[T] extends Condition[T] {
    def refValue: T
  }

  abstract class WildCard[T] extends Condition[T] {
    override def matches(v: T): Boolean = true
    override def toString(): String = { attributeName + "=#" }
  }

  abstract class LowerThanNumCondition[T: Numeric] extends ConditionOneValue[T] {
    override def matches(v: T): Boolean = { v <= refValue }
    override def toString(): String = { attributeName + "<=" + refValue }
  }

  abstract class GreaterThanNumCondition[T: Numeric] extends ConditionOneValue[T] {
    override def matches(v: T): Boolean = { v >= refValue }
    override def toString(): String = { attributeName + ">=" + refValue }
  }

  abstract class EqualToCondition[T] extends ConditionOneValue[T] {
    override def matches(v: T): Boolean = (v == refValue)
    override def toString(): String = { attributeName + "=" + refValue }
  }

  case class WildCardIntCondition(attributeName: String) extends WildCard[Int]
  case class LowerThanIntCondition(attributeName: String, refValue: Int) extends LowerThanNumCondition[Int]
  case class GreaterThanIntCondition(attributeName: String, refValue: Int) extends GreaterThanNumCondition[Int]
  case class EqualToIntCondition(attributeName: String, refValue: Int) extends EqualToCondition[Int]

  case class WildCardFloatCondition(attributeName: String) extends WildCard[Double]
  case class LowerThanFloatCondition(attributeName: String, refValue: Double) extends LowerThanNumCondition[Double]
  case class GreaterThanFloatCondition(attributeName: String, refValue: Double) extends GreaterThanNumCondition[Double]
  case class EqualToFloatCondition(attributeName: String, refValue: Double) extends EqualToCondition[Double]

  case class WildCardBoolCondition(attributeName: String) extends WildCard[Boolean]
  case class EqualToBoolCondition(attributeName: String, refValue: Boolean) extends EqualToCondition[Boolean]

  object Condition {

    /**
     * Factory to create Conditions for various variable types
     */
    def apply[T](v: Variable[T], rng: scala.util.Random): Condition[_] = v.value match {
      case value: Int     ⇒ createIntegerCondition(v.asInstanceOf[Variable[Int]], rng)
      case value: Double  ⇒ createFloatCondition(v.asInstanceOf[Variable[Double]], rng)
      case value: Boolean ⇒ createBoolCondition(v.asInstanceOf[Variable[Boolean]], rng)
      // TODO ???
      case _              ⇒ throw new IllegalArgumentException("Sorry, unable to create a condition for value " + v.value)
    }

    def createIntegerCondition(v: Variable[Int], rng: scala.util.Random): Condition[Int] = {
      val r: Int = rng.nextInt(100)
      if (r <= 25) { LowerThanIntCondition(v.prototype.simpleName, v.value + 1) }
      else if (r <= 50) { GreaterThanIntCondition(v.prototype.simpleName, v.value - 1) }
      else if (r <= 60) { EqualToIntCondition(v.prototype.simpleName, v.value) }
      else { WildCardIntCondition(v.prototype.simpleName) }
    }

    def createFloatCondition(v: Variable[Double], rng: scala.util.Random): Condition[Double] = {
      val r: Int = rng.nextInt(100)
      if (r <= 25) { LowerThanFloatCondition(v.prototype.simpleName, v.value + 1) }
      else if (r <= 50) { GreaterThanFloatCondition(v.prototype.simpleName, v.value - 1) }
      else if (r <= 60) { EqualToFloatCondition(v.prototype.simpleName, v.value) }
      else { WildCardFloatCondition(v.prototype.simpleName) }
    }

    def createBoolCondition(v: Variable[Boolean], rng: scala.util.Random): Condition[Boolean] = {
      val r: Int = rng.nextInt(100)
      if (r <= 60) { EqualToBoolCondition(v.prototype.simpleName, v.value) }
      else { WildCardBoolCondition(v.prototype.simpleName) }
    }

  }

  /*
  trait Action[T] {
    def prototype: Val[T]
    def value: T
    override def toString: String = attributeName + "=" + value
  }

  case class ActionValue[T](prototype: Val[T], value: T) extends Action[T]
  */

  abstract class AbstractClassifier {
    def age: Int
    def conditions: Array[Condition[_]]
    def actions: Array[Variable[Q] forSome { type Q }]
    override def toString: String = "if " + conditions.toList + " then " + actions.toList + "(" + age + ")"

    /**
     * Returns true if the conditions of the classifier are verified by the entity passed as a parameter
     */
    def matches(entity: Entity): Boolean = conditions.zipWithIndex.forall { case (c, i) ⇒ c.acceptsUnsafe(entity.characteristics(i)) } // matches .value

    /**
     * Applies the action of the classifier over this entity.
     * Returns a copy of this entity with different values for actions
     */
    def actUpon(entity: Entity): Entity = entity.copy(actions = actions.map(av ⇒ av).toArray) // TODO clone ???

  }

  case class ClassifierRule(
    conditions: Array[Condition[_]],
    actions:    Array[Variable[Q] forSome { type Q }],
    age:        Int
  ) extends AbstractClassifier

  object ClassifierRule {

    /**
     * Generates a random rule matching this entity
     */
    def apply(e: Entity, _actions: Seq[Genes.Gene[_]], context: Context)(implicit rng: RandomProvider, newFile: NewFile, fileService: FileService): ClassifierRule = {

      ClassifierRule(
        // generate random conditions matching entity characteristics
        e.characteristics.map(c ⇒ Condition(c, rng())).toArray,
        // micro actions are random
        _actions.map(a ⇒ Variable.unsecure(a.prototype, a.makeRandomValue(context))).toArray, // TODO
        0
      )
    }
  }

  case class EntityAttributeValue[T](
    name:  String,
    value: T
  )

  case class Entity(
    id:              Int,
    characteristics: Array[Variable[_]],
    actions:         Array[Variable[_]]
  ) {
    override def toString: String = "Entity " + id + " " + characteristics.toList + " " + actions.toList
  }

  /**
   * Entry point for the method: applies MicroLCS
   * with a list of input characteristics for entities,
   * actions to tune for each entity, and
   * a count of iterations to drive.
   */
  def MicroLCS(
    microCharacteristics: Seq[Val[Array[T]] forSome { type T }],
    microActions:         Seq[Genes.Gene[_]],
    iterations:           Int,
    evaluation:           Puzzle
  )(implicit newFile: NewFile, fileService: FileService): Puzzle = {

    //
    // rng: RandomProvider,

    // the first step is to decode the initial lists of characteristics as lists of individuals.
    val decodeIndividuals = DecodeEntities(microCharacteristics, microActions)

    val doMatching = Matching(microActions)

    val encodeIndividuals = EncodeEntities(microCharacteristics, microActions)

    decodeIndividuals -- doMatching -- encodeIndividuals -- evaluation
  }

}

