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

import scala.reflect.runtime.universe._
import org.openmole.core.expansion.{ Condition, FromContext }
import org.openmole.plugin.method.microlcs.DecodeEntities.{ varMax, varMin }

import Numeric.Implicits._
import Ordering.Implicits._
import scala.reflect.ClassTag

package object microlcs {

  val namespaceMicroLCS = Namespace("microlcs")

  // this value will contain the set of rules
  val varRules = Val[Array[ClassifierRule]]("rules", namespace = namespaceMicroLCS)

  val varIterations = Val[Int]("iterations", namespace = namespaceMicroLCS)

  implicit def scope = DefinitionScope.Internal

  trait Condition[T] {
    def attributeName: String
    def matches(v: T): Boolean
    def accepts(v: Variable[T]): Boolean = (v.prototype.simpleName == attributeName) && matches(v.value)
    def acceptsUnsafe(v: Variable[_]) = v match {
      case vcasted: Variable[T] ⇒ accepts(vcasted)
      case _                    ⇒ throw new IllegalArgumentException("expecting another type for " + this)
    }
    def subsums(other: Condition[T]): Boolean
    def subsumsUnsafe(other: Condition[_]): Boolean = other match {
      case otherCasted: Condition[T] ⇒ subsums(otherCasted)
      case _                         ⇒ throw new IllegalArgumentException("Can only compare similar conditions")
    }
  }

  abstract class ConditionOneValue[T] extends Condition[T] {
    def refValue: T
  }

  abstract class WildCard[T] extends Condition[T] {
    override def matches(v: T): Boolean = true
    override def toString(): String = { attributeName + "==#" }
    override def subsums(other: Condition[T]) = true
  }

  abstract class LowerThanNumCondition[T: Numeric] extends ConditionOneValue[T] {
    override def matches(v: T): Boolean = { v <= refValue }
    override def toString(): String = { attributeName + "<=" + refValue }
    override def subsums(other: Condition[T]) = other match {
      case lt: LowerThanNumCondition[T]  ⇒ refValue >= lt.refValue
      case _: GreaterThanNumCondition[T] ⇒ false
      case _: EqualToCondition[T]        ⇒ false
      case _: WildCard[T]                ⇒ false
      case _                             ⇒ false // TODO warn ???
    }

  }

  abstract class GreaterThanNumCondition[T: Numeric] extends ConditionOneValue[T] {
    override def matches(v: T): Boolean = { v >= refValue }
    override def toString(): String = { attributeName + ">=" + refValue }
    override def subsums(other: Condition[T]) = other match {
      case lt: GreaterThanNumCondition[T] ⇒ refValue <= lt.refValue
      case _: LowerThanNumCondition[T]    ⇒ false
      case _: EqualToCondition[T]         ⇒ false
      case _: WildCard[T]                 ⇒ false
      case _                              ⇒ false // TODO warn ???
    }
  }

  abstract class EqualToCondition[T] extends ConditionOneValue[T] {
    override def matches(v: T): Boolean = (v == refValue)
    override def toString(): String = { attributeName + "==" + refValue }
    override def subsums(other: Condition[T]) = other match {
      case _: GreaterThanNumCondition[T] ⇒ false
      case _: LowerThanNumCondition[T]   ⇒ false
      case eq: EqualToCondition[T]       ⇒ refValue == eq.refValue
      case _: WildCard[T]                ⇒ false
      case _                             ⇒ false // TODO warn ???
    }
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

    // TODO String

    def mutateInt(c: Condition[Int], min: Int, max: Int, rng: scala.util.Random): Condition[Int] = {

      val refVal = c match {
        case ov: ConditionOneValue[Int] ⇒ ov.refValue
        case _                          ⇒ min + rng.nextInt(max - min) // TODO what is a good value ???
      }
      val refName = c.attributeName

      val r: Int = rng.nextInt(100)
      if (r <= 25) {
        LowerThanIntCondition(refName, refVal)
      }
      else if (r <= 50) {
        GreaterThanIntCondition(refName, refVal)
      }
      else if (r <= 60) {
        EqualToIntCondition(refName, refVal)
      }
      else {
        WildCardIntCondition(refName)
      }

    }

    def mutateDouble(c: Condition[Double], min: Double, max: Double, rng: scala.util.Random): Condition[Double] = {

      val refVal = c match {
        case ov: ConditionOneValue[Double] ⇒ ov.refValue
        case _                             ⇒ rng.nextDouble() * (max - min) + min
      }
      val refName = c.attributeName

      val r: Int = rng.nextInt(100)
      if (r <= 25) {
        LowerThanFloatCondition(refName, refVal)
      }
      else if (r <= 50) {
        GreaterThanFloatCondition(refName, refVal)
      }
      else if (r <= 60) {
        EqualToFloatCondition(refName, refVal)
      }
      else {
        WildCardFloatCondition(refName)
      }

    }

    def mutateBoolean(c: Condition[Boolean], rng: scala.util.Random): Condition[Boolean] = c match {

      case w: WildCardBoolCondition ⇒ EqualToBoolCondition(c.attributeName, rng.nextBoolean())
      case eq: EqualToBoolCondition ⇒
        if (rng.nextBoolean()) {
          EqualToBoolCondition(c.attributeName, !eq.refValue)
        }
        else {
          WildCardBoolCondition(c.attributeName)
        }
    }

    def mutate[T: ClassTag](c: Condition[T], min: Double, max: Double)(implicit rng: RandomProvider): Condition[T] = c match {
      case LowerThanIntCondition(_, _) | GreaterThanIntCondition(_, _) | EqualToIntCondition(_, _) | WildCardIntCondition(_) ⇒ mutateInt(c.asInstanceOf[Condition[Int]], min.toInt, max.toInt, rng())
      case LowerThanFloatCondition(_, _) | GreaterThanFloatCondition(_, _) | EqualToFloatCondition(_, _) | WildCardFloatCondition(_) ⇒ mutateDouble(c.asInstanceOf[Condition[Double]], min, max, rng())
      case EqualToBoolCondition(_, _) | WildCardBoolCondition(_) ⇒ mutateBoolean(c.asInstanceOf[Condition[Boolean]], rng())
      case _ ⇒ throw new IllegalArgumentException("oops, we are not able to mutate gene " + c)
    }

  }

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
      _actions: Seq[Genes.Gene[_]],
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

    def mutate(r: ClassifierRule, microActions: Seq[Genes.Gene[_]], mins: Array[Double], maxs: Array[Double], context: Context)(implicit rng: RandomProvider, newFile: NewFile, fileService: FileService): ClassifierRule = {
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
    evaluation:           Puzzle,
    microMinimize:        Seq[Val[Double]],
    microMaximize:        Seq[Val[Double]]
  )(implicit newFile: NewFile, fileService: FileService): Puzzle = {

    //
    // rng: RandomProvider,

    // the first step is to decode the initial lists of characteristics as lists of individuals.
    val decodeIndividuals = DecodeEntities(microCharacteristics, microActions)
    val sDecodeIndividuals = Slot(decodeIndividuals)

    val doMatching = Matching(microActions)
    val cDoMatching = Capsule(doMatching)
    val sDoMatching = Slot(cDoMatching)

    val encodeIndividuals = EncodeEntities(microCharacteristics, microActions)
    val sEncodeIndividuals = Slot(encodeIndividuals)

    val evaluate = Evaluate(microMinimize, microMaximize)
    val sEvaluate = Slot(evaluate)

    val subsume = Subsumption(microMinimize, microMaximize)

    val evolve = Evolve(microActions, microCharacteristics, 100)
    val sEvolve = Slot(evolve)

    val delete = Delete(100)
    val cDelete = Capsule(delete)
    val sDelete = Slot(cDelete)

    //val sDoMatchingLoop = Slot(cDoMatching)

    val dispatch = ExplorationTask(DispatchEntities(10))

    val aggregate = AggregateResults()

    val beginLoop = EmptyTask() set (
      name := "beginLoop",
      (inputs, outputs) += (DecodeEntities.varEntities, varRules, varIterations, DecodeEntities.varMin, DecodeEntities.varMax)
    )

    val beginLoopCapsule = Capsule(beginLoop, strain = true)
    val beginLoopExecInit = Slot(beginLoopCapsule)
    val beginLoopExecLoop = Slot(beginLoopCapsule)

    val export = ExportRules(microCharacteristics, microActions, microMinimize, microMaximize)

    (
      (
        (
          sDecodeIndividuals --
          beginLoopExecInit --
          dispatch -< (
            sDoMatching --
            sEncodeIndividuals --
            evaluation --
            sEvaluate) >-
            aggregate --
            subsume --
            sEvolve --
            sDelete
        ) &

            // convey rules, iteration, micro entities and other information over the evaluation
            (sEncodeIndividuals -- sEvaluate) &

            // loop
            (sDelete -- (beginLoopExecLoop when "microlcs$iterations < " + iterations))

      ) -- export
    )

    /*
    (
      (sDecodeIndividuals -- sDoMatching -- sEncodeIndividuals -- evaluation -- sEvaluate -- subsume -- sEvolve -- sDelete) &

      // convey rules, iteration, micro entities and other information over the evaluation
      (sEncodeIndividuals -- sEvaluate) &

      // loop
      (sDelete -- (sDoMatchingLoop when "microlcs$iterations < " + iterations))

    )*/

  }

}

