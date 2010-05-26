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

package org.openmole.core.workflow.model.execution.batch;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.openmole.commons.exception.InternalProcessingError;
import org.openmole.commons.exception.UserBadDataError;

public interface IUsageControl {
	IAccessToken tryGetToken(long milliSeconds, TimeUnit unit) throws InterruptedException, TimeoutException;
	IAccessToken waitAToken() throws InterruptedException;
	void releaseToken(IAccessToken token) throws InternalProcessingError;
	IAccessToken getAccessTokenInterruptly();
	int getLoad();

}
