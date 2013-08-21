/*
 * Copyright (C) 10/06/13 Romain Reuillon
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
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.core.implementation.mole

import org.openmole.core.model.mole._
import org.openmole.misc.workspace._
import org.openmole.core.model.data._
import org.openmole.core.model.execution.{ UnauthenticatedEnvironment, AuthenticationProvider, Environment }

object PartialMoleExecution {
  def apply(
    mole: IMole,
    sources: Iterable[(ICapsule, ISource)] = Iterable.empty,
    hooks: Iterable[(ICapsule, IHook)] = Iterable.empty,
    environments: Map[ICapsule, UnauthenticatedEnvironment] = Map.empty,
    grouping: Map[ICapsule, Grouping] = Map.empty,
    profiler: Profiler = Profiler.empty,
    seed: Long = Workspace.newSeed): PartialMoleExecution = new PartialMoleExecution(
    mole,
    sources groupBy { case (c, _) ⇒ c } map { case (c, ss) ⇒ c -> ss.map(_._2) } withDefault { _ ⇒ List.empty },
    hooks groupBy { case (c, _) ⇒ c } map { case (c, hs) ⇒ c -> hs.map(_._2) } withDefault { _ ⇒ List.empty },
    environments,
    grouping,
    profiler,
    seed)
}

class PartialMoleExecution(
    val mole: IMole,
    val sources: Sources = Sources.empty,
    val hooks: Hooks = Hooks.empty,
    val environments: Map[ICapsule, UnauthenticatedEnvironment] = Map.empty,
    val grouping: Map[ICapsule, Grouping] = Map.empty,
    val profiler: Profiler = Profiler.empty,
    val seed: Long = Workspace.newSeed) extends IPartialMoleExecution {

  def authenticate(unauthenticated: Traversable[UnauthenticatedEnvironment])(implicit moleExecutionContext: ExecutionContext) =
    unauthenticated.map(e ⇒ e -> e(moleExecutionContext.authentications)).toMap

  def toExecutionGetEnvironments(implicit implicits: Context = Context.empty, moleExecutionContext: ExecutionContext = ExecutionContext.local) = {
    val authenticatedEnvironments = authenticate(environments.values)

    (new MoleExecution(mole,
      sources,
      hooks,
      environments.map { case (k, v) ⇒ k -> authenticatedEnvironments(v) },
      grouping,
      profiler,
      seed)(implicits, moleExecutionContext),
      authenticatedEnvironments)
  }

  def toExecution(implicit implicits: Context = Context.empty, moleExecutionContext: ExecutionContext = ExecutionContext.local) =
    toExecutionGetEnvironments._1
}
