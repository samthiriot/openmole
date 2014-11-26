package org.openmole.gui.ext.panelui

import scalatags.Text.tags
import scalatags.generic.TypedTag
import org.scalajs.dom

/*
 * Copyright (C) 14/11/14 // mathieu.leclaire@openmole.org
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

/*object PanelUI {
  def apply(): PanelUI = new PanelUI {
    def run() = tags.html.render
  }
  def unapply(): Option[PanelUI] = Some(new PanelUI {
    def run() = tags.html.render
  })
}*/

abstract class PanelUI {
  type FormTag = TypedTag[dom.Element, dom.Element, dom.Node]
  def run: FormTag
}