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
package org.openmole.core.workflow.implementation.task;

import java.util.Collection;
import org.openmole.core.workflow.implementation.data.Data;
import org.openmole.core.workflow.model.data.IData;
import org.openmole.core.workflow.model.resource.IResource;
import org.openmole.commons.exception.InternalProcessingError;
import org.openmole.commons.exception.UserBadDataError;
import org.openmole.core.workflow.model.execution.IProgress;
import org.openmole.core.workflow.model.job.IContext;
import org.openmole.core.workflow.model.mole.IExecutionContext;
import org.openmole.core.workflow.model.plan.IExploredPlan;
import org.openmole.core.workflow.model.plan.IPlan;
import org.openmole.core.workflow.model.task.IExplorationTask;
import org.openmole.core.workflow.model.task.annotations.Output;
import org.openmole.commons.aspect.caching.ChangeState;
import org.openmole.commons.aspect.caching.SoftCachable;

public class ExplorationTask extends GenericTask implements IExplorationTask {

    @Output
    final static public IData<IExploredPlan> ExploredPlan = new Data<IExploredPlan>("ExploredPlan", IExploredPlan.class);

    private IPlan plan;

    public ExplorationTask(String name) throws UserBadDataError, InternalProcessingError {
        super(name);
    }

    public ExplorationTask(String name, IPlan plan) throws UserBadDataError, InternalProcessingError {
        super(name);
        setPlan(plan);
    }

    //If input prototype as the same name as the output it is erased
    @Override
    protected void process(IContext context, IExecutionContext localFileCache, IProgress progress) throws UserBadDataError, InternalProcessingError, InterruptedException {
        context.putVariable(ExploredPlan.getPrototype(), plan.build(context));
    }

    /* (non-Javadoc)
     * @see org.openmole.methods.task.IExploration#setDesign(org.openmole.core.task.ExperimentalDesign)
     */
    @ChangeState
    public void setPlan(IPlan plan) {
        this.plan = plan;
    }

    /* (non-Javadoc)
     * @see org.openmole.methods.task.IExploration#getDesign()
     */
    @Override
    public IPlan getPlan() {
        return plan;
    }

    @SoftCachable
    @Override
    public Collection<IResource> getResources() throws InternalProcessingError, UserBadDataError {
        Collection<IResource> resourcesCache = super.getResources();

        for(IResource resource: getPlan().getResources()) {
            resourcesCache.add(resource);
        }

        return resourcesCache;
    }

}
