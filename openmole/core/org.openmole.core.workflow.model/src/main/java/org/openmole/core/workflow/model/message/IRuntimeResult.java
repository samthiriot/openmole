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

package org.openmole.core.workflow.model.message;

import java.io.File;

import org.openmole.core.workflow.model.file.IURIFile;
import org.openmole.core.workflow.model.job.IContext;
import org.openmole.core.workflow.model.job.IMoleJobId;
import org.openmole.commons.tools.structure.Duo;
import org.openmole.commons.tools.io.IHash;

public interface IRuntimeResult extends IRuntimeMessage {
	void setException(Throwable exception);
	Throwable getException();
	
	void setStdErr(IURIFile stdErr, IHash hash);
	Duo<IURIFile, IHash> getStdErr();

	void setStdOut(IURIFile stdOut, IHash hash);
	Duo<IURIFile, IHash> getStdOut();
	
	public void putResult(IMoleJobId jobId, IContext context);
	public boolean containsResultForJob(IMoleJobId jobId);
	public IContext getContextForJob(IMoleJobId jobId);
	
	Duo<IURIFile, IHash> getTarResult();
	void setTarResult(IURIFile tarResult, IHash hash);
	void addFileName(String hash, File filePath, boolean isDirectory);
	Duo<File, Boolean> getFileInfoForEntry(String hash);
	
}
