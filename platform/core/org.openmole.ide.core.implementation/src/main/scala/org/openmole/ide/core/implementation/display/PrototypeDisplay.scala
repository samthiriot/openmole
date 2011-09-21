/*
 * Copyright (C) 2011 <mathieu.leclaire at openmole.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.openmole.ide.core.implementation.display

import org.openide.util.Lookup
import scala.collection.mutable.HashSet
import scala.collection.JavaConversions._
import org.openmole.ide.misc.exception.GUIUserBadDataError
import org.openmole.ide.core.model.display.IDisplay
import org.openmole.ide.core.model.panel.IPrototypePanelUI
import org.openmole.ide.core.model.factory.IPrototypeFactoryUI
import org.openmole.ide.core.implementation.dataproxy._
import org.openmole.ide.core.model.dataproxy._

object PrototypeDisplay extends IDisplay{
  private var modelPrototypes = new HashSet[PrototypeDataProxyFactory]
  var currentPanel: Option[IPrototypePanelUI[_]] = None
  var currentDataProxy: Option[IPrototypeDataProxyUI] = None
  
  Lookup.getDefault.lookupAll(classOf[IPrototypeFactoryUI[_]]).foreach(f=>{modelPrototypes += new PrototypeDataProxyFactory(f)})
  
  override def setCurrentDataProxy(pID: Int) = currentDataProxy = Some(Proxys.prototype(pID))
  
  override def implementationClasses = modelPrototypes
  
  def  buildPanelUI= {
    currentPanel = Some(currentDataProxy.get.dataUI.buildPanelUI)
    currentPanel.get
  }
  
  override def saveContent(name: String) = {
    currentDataProxy.get.dataUI = currentPanel.getOrElse(throw new GUIUserBadDataError("No panel to print for entity " + name)).saveContent(name)
    if (Displays.initMode) Proxys.addPrototypeElement(currentDataProxy.get)
  }
}