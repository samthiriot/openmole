/*
 * Copyright (C) 2011 reuillon
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

package org.openmole.core.model.hook

import org.openmole.core.model.job.IMoleJob
import org.openmole.core.model.job.State.State
import org.openmole.core.model.mole.ICapsule

trait IMoleExecutionHook extends IHook {
  def stateChanged(moleJob: IMoleJob, newState: State, oldState: State)
  def executionStarting
  def executionFinished
  def jobFinished(moleJob: IMoleJob, capsule: ICapsule) = {}
  def jobStarting(moleJob: IMoleJob, capsule: ICapsule) = {}
}
