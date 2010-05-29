/*
 *  Copyright (C) 2010 reuillon
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.core.implementation.execution.batch;

import org.openmole.core.implementation.execution.EnvironmentDescription;
import org.openmole.core.model.execution.batch.IBatchEnvironment;
import org.openmole.core.model.execution.batch.IBatchEnvironmentDescription;

/**
 *
 * @author reuillon
 */
public abstract class BatchEnvironmentDescription<ENV extends IBatchEnvironment> extends EnvironmentDescription<ENV> implements IBatchEnvironmentDescription<ENV>{

}
