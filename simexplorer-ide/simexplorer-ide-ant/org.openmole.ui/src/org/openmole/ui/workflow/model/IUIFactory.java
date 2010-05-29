/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.ui.workflow.model;

import org.openmole.core.model.capsule.IGenericTaskCapsule;
import org.openmole.misc.eventdispatcher.IObjectConstructedAsynchronousListener;

/**
 *
 * @author Mathieu Leclaire <mathieu.leclaire@openmole.fr>
 */
public interface IUIFactory<T> extends IObjectConstructedAsynchronousListener<T> {
    //ICapsuleModelUI createTaskCapsuleModel(IGenericTaskCapsule gtc);
    IGenericTaskModelUI createTaskModelInstance(Class<? extends IGenericTaskModelUI> modelClass);

    
}
