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
import org.openmole.core.expansion.FromContext
import org.openmole.plugin.method.microlcs.DecodeEntities.{ varMax, varMin }

import Numeric.Implicits._
import Ordering.Implicits._
import scala.reflect.ClassTag

package object microlcs {

  val namespaceMicroLCS = Namespace("microlcs")

  // this value will contain the set of rules
  val varRules = Val[Array[ClassifierRule]]("rules", namespace = namespaceMicroLCS)
  // refers to a list of rules applied on a simultion, each rule corresponding one entity
  val varRulesApplied = Val[Array[ClassifierRule]]("rules", namespace = namespaceMicroLCS)

  val varIterations = Val[Int]("iterations", namespace = namespaceMicroLCS)

  val varPlans = Val[Array[MacroGene]]("plans", namespace = namespaceMicroLCS)
  val varPlansBefore = Val[Array[MacroGene]]("plans_before", namespace = namespaceMicroLCS)

  val varPlanSimulated = Val[MacroGene]("plan_simulated", namespace = namespaceMicroLCS)

  implicit def scope = DefinitionScope.Internal

  case class MicroCharacteristic(prototype: Val[Array[T]] forSome { type T })

  object MicroCharacteristic {

    implicit def valDoubleIsMicroCharacteristic(vd: Val[Array[Double]]) = MicroCharacteristic(vd)
    implicit def valIntIsMicroCharacteristic(vi: Val[Array[Int]]) = MicroCharacteristic(vi)
    implicit def valBooleanIsMicroCharacteristic(vb: Val[Array[Boolean]]) = MicroCharacteristic(vb)
    implicit def valStringIsMicroCharacteristic(vs: Val[Array[String]]) = MicroCharacteristic(vs)

    //implicit def microCharacteristicIsVal(m: MicroCharacteristic): Val[Array[T]] forSome { type T } = m.prototype

  }

  type MicroCharacteristics = Seq[MicroCharacteristic]

  /**
   * Entry point for the method: applies MicroLCS
   * with a list of input characteristics for entities,
   * actions to tune for each entity, and
   * a count of iterations to drive.
   */
  def MicroLCS(
    microCharacteristics: MicroCharacteristics, // Seq[Val[Array[T]] forSome { type T }],
    microActions:         Seq[MicroGenes.Gene[_]],
    iterations:           Int,
    evaluation:           Puzzle,
    microMinimize:        Seq[Val[Double]],
    microMaximize:        Seq[Val[Double]],
    macroMinimize:        Seq[Val[Double]],
    macroMaximize:        Seq[Val[Double]]
  )(implicit newFile: NewFile, fileService: FileService): Puzzle = {

    //
    // rng: RandomProvider,

    val simulationCapsuleMicro = evaluation // Capsule(MoleTask(evaluation))

    // the first step is to decode the initial lists of characteristics as lists of individuals.
    val decodeIndividuals = DecodeEntities(microCharacteristics, microActions)
    val sDecodeIndividuals = Slot(decodeIndividuals)

    val doMatching = Matching(microActions, false)
    val cDoMatching = Capsule(doMatching)
    val sDoMatching = Slot(cDoMatching)

    val encodeIndividuals = EncodeEntities(microCharacteristics, microActions)
    val sEncodeIndividuals = Slot(encodeIndividuals)

    val evaluate = Evaluate(microMinimize, microMaximize)
    val sEvaluate = Slot(evaluate)

    val subsume = Subsumption(microMinimize, microMaximize)

    val evolve = Evolve(microActions, microCharacteristics, 100)
    val sEvolve = Slot(evolve)

    val delete = Delete(200)
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

    val generateInitPlans = GenerateInitPlans(microMinimize, microMaximize, 500)
    val generateInitPlansSlot = Slot(generateInitPlans)

    val dispatchPlans = ExplorationTask(SamplePlans())
    val cDispatchPlans = Capsule(dispatchPlans)
    val sDispatchPlansInit = Slot(cDispatchPlans)
    val sDispatchPlansLoop = Slot(cDispatchPlans)

    val matchingPlans = Matching(microActions, true) set ((inputs, outputs) += (varPlanSimulated, varPlansBefore))
    val encodeIndividualsPlans = EncodeEntities(microCharacteristics, microActions) set ((inputs, outputs) += (varPlanSimulated, varPlansBefore))
    val simulationMacro = evaluation.copy()
    val simulationCapsuleMacro = Capsule(MoleTask(evaluation) set (
      (inputs, outputs) += (varPlanSimulated, varIterations, varRules, DecodeEntities.varEntities, DecodeEntities.varMin, DecodeEntities.varMax, varPlansBefore)
    )
    )

    val evaluatePlan = EvaluateMacro(microMinimize, microMaximize, macroMinimize, macroMaximize)

    val aggregatePlans = AggregateResultsPlan()

    val evolvePlans = EvolvePlans(100, microActions)
    val sEvolvePlans = Slot(evolvePlans)

    (

      (
        sDecodeIndividuals -- beginLoopExecInit -- dispatch
        -< (sDoMatching -- sEncodeIndividuals -- simulationCapsuleMicro -- sEvaluate) >-
        aggregate -- subsume -- sEvolve -- sDelete
      ) & // convey rules, iteration, micro entities and other information over the evaluation
        (sEncodeIndividuals -- sEvaluate) &
        // loop
        (sDelete -- (beginLoopExecLoop when "microlcs$iterations < " + iterations)) &
        // continue
        (sDelete -- (generateInitPlansSlot when "microlcs$iterations == " + iterations)) &
        (generateInitPlansSlot -- sDispatchPlansInit -< matchingPlans -- encodeIndividualsPlans -- simulationCapsuleMacro -- evaluatePlan >- aggregatePlans -- sEvolvePlans) &
        (sEvolvePlans -- (sDispatchPlansLoop when "microlcs$iterations < " + (iterations * 2)))

    // propagate varPlansBefore
    // & (sDispatchPlansInit -- sEvolvePlans)

    ) // TODO !!! -- export
    //-- evaluation

    // TODO varPlansBefore

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

