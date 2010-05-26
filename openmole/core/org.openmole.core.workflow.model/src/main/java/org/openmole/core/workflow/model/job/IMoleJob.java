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

package org.openmole.core.workflow.model.job;

import org.openmole.core.workflow.model.mole.IExecutionContext;
import java.io.File;
import org.openmole.commons.exception.ExecutionException;


import org.openmole.commons.exception.InternalProcessingError;
import org.openmole.commons.exception.UserBadDataError;
import org.openmole.core.workflow.model.execution.IProgress;
import org.openmole.core.workflow.model.task.IGenericTask;
import org.openmole.core.workflow.model.resource.IResource;

public interface IMoleJob {

    public final static String stateChanged = "stateChanged";

    IGenericTask getTask();

    State getState();

    boolean isFinished();

    IContext getContext();
    
    void perform(IExecutionContext executionContext) throws InterruptedException;
    void finished(IContext executionJob) throws UserBadDataError, InternalProcessingError;

    void rethrowException(IContext context) throws ExecutionException;

    ITicket getTicket();

    IProgress getProgress();

    Iterable<IResource> getConsumedRessources() throws InternalProcessingError, UserBadDataError;

    Iterable<File> getInputFiles() throws InternalProcessingError;

    IMoleJobId getId();

    void cancel() throws InternalProcessingError, UserBadDataError;
    
}
