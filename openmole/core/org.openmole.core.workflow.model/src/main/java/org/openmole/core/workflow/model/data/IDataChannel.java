/*
 *  Copyright (C) 2010 Romain Reuillon
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

package org.openmole.core.workflow.model.data;

import org.openmole.core.workflow.model.capsule.IGenericTaskCapsule;
import org.openmole.core.workflow.model.job.ITicket;
import java.util.Set;
import org.openmole.commons.exception.InternalProcessingError;
import org.openmole.core.workflow.model.job.IContext;
import org.openmole.core.workflow.model.mole.IMoleExecution;
import org.openmole.commons.exception.UserBadDataError;

/**
 *
 * @author reuillon
 */
public interface IDataChannel {

    IGenericTaskCapsule<?, ?> getStart();
    IGenericTaskCapsule<?, ?> getEnd();

    void setStart(IGenericTaskCapsule start);
    void setEnd(IGenericTaskCapsule end);

    Iterable<String> getVariableNames();
    void add(IPrototype prototype);
    void add(String name);

    Iterable<IData> getData() throws InternalProcessingError, UserBadDataError;

    void provides(IContext context, ITicket ticket, Set<String> toClone, IMoleExecution moleExecution) throws InternalProcessingError, UserBadDataError;
    IContext consums(IContext context, ITicket ticket, Set<String> toClonne, IMoleExecution moleExecution) throws InternalProcessingError;

}
