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

import scala.annotation.tailrec

/**
 * Trait which characterizes what has a performance evalauted on several criteria.
 * Brings Pareto dominance and others
 */
trait HasMultiObjectivePerformance {

  var performance: Seq[Seq[Double]]

  def applications(): Int = {
    if (performance.isEmpty) { 0 }
    else { performance(0).length }
  }

  def dominatesPareto(other: HasMultiObjectivePerformance): Boolean = {
    val perfMine = performanceAggregated()
    val perfOther = other.performanceAggregated()
    val zipped = perfMine zip perfOther
    zipped.forall { case (m, o) ⇒ m <= o } && zipped.exists { case (m, o) ⇒ m < o }
  }

  def strictlyDominated(other: HasMultiObjectivePerformance): Boolean = {
    val perfMine = performanceAggregated()
    val perfOther = other.performanceAggregated()
    val zipped = perfMine zip perfOther
    zipped.forall { case (m, o) ⇒ m >= o }
  }

  def performanceAggregated() = performance.map(vals ⇒ vals.sum / vals.length)

  def performanceAggregated(i: Int) = performance(i).sum / performance(i).length

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

  def performanceToString() = {
    if (performance.isEmpty) "(never evaluated)"
    else "(" + performance(0).length + ") [" + performanceAggregated().map(v ⇒ v.toString).mkString(",") + "]"
  }

}

object HasMultiObjectivePerformance {

  @tailrec
  def detectParetoFrontRec[T <: HasMultiObjectivePerformance](elems: List[T], acc: List[T]): Iterable[T] = elems match {
    case Nil ⇒ acc
    case e :: tail ⇒
      if (tail.exists(e.strictlyDominated(_)) || acc.exists(e.strictlyDominated(_))) {
        detectParetoFrontRec(tail, acc)
      }
      else {
        detectParetoFrontRec(tail, acc ++ List(e))
      }
  }

  def detectParetoFront[T <: HasMultiObjectivePerformance](elems: Iterable[T]): Iterable[T] = detectParetoFrontRec(elems.toList, List())

  @tailrec
  def detectParetoFrontsRec[T <: HasMultiObjectivePerformance](elems: Set[T], acc: List[Iterable[T]]): List[Iterable[T]] = {
    if (elems.isEmpty) {
      acc
    }
    else {
      val nthParetoFront = detectParetoFront(elems)
      val remaining = elems.filterNot(nthParetoFront.toSet)
      detectParetoFrontsRec(remaining, acc ++ List(nthParetoFront))
    }
  }

  /**
   * Detects the successive Pareto fronts and returns them in order (the first being the best)
   */
  def detectParetoFronts[T <: HasMultiObjectivePerformance](elems: Array[T]): Seq[Iterable[T]] = detectParetoFrontsRec(elems.toSet, List()).toSeq

  def paretoFrontsToPrettyString[T <: HasMultiObjectivePerformance](fronts: Seq[Iterable[T]]): String =
    fronts.zipWithIndex
      .map {
        case (f, i) ⇒ (i+1) + "th Pareto front:\n" +
          f.toList.sortWith(_.performanceAggregated(0) < _.performanceAggregated(0))
          .map(_.toString)
          .mkString("\t\n")
      }.mkString("\n\n")

}