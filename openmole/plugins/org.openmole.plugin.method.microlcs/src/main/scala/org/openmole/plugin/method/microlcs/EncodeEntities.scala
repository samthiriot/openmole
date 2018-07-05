/*
 * Copyright (C) 2018 Samuel Thiriot
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
package org.openmole.plugin.method.microlcs

import org.openmole.core.context.{ Context, Namespace, Val, Variable }
import org.openmole.core.expansion.FromContext
import org.openmole.core.workflow.builder.DefinitionScope
import org.openmole.core.workflow.sampling.Sampling
import org.openmole.core.workflow.task.ClosureTask
import org.openmole.core.workflow.tools.ScalarOrSequenceOfDouble
import org.openmole.tool.logger.JavaLogger
import org.openmole.core.workflow.dsl._
import org.openmole.core.fileservice.FileService
import org.openmole.core.workspace.NewFile
import scala.reflect.runtime.universe._

object EncodeEntities {

  // ugly.
  // but seriously, spending hours for manipulating generics is enough.
  def toArrayTyped[T](elems: Seq[Any]): Array[_] = elems(0) match {
    case _: Double  ⇒ elems.asInstanceOf[Seq[Double]].toArray
    case _: Integer ⇒ elems.asInstanceOf[Seq[Integer]].toArray
    case _: Boolean ⇒ elems.asInstanceOf[Seq[Boolean]].toArray
    case _: String  ⇒ elems.asInstanceOf[Seq[String]].toArray
    case _: Object  ⇒ elems.toArray
  }

  def apply[T](
    _characteristics: Seq[Val[Array[Q]] forSome { type Q }],
    _actions:         Seq[Genes.Gene[_]])(implicit name: sourcecode.Name, definitionScope: DefinitionScope, newFile: NewFile, fileService: FileService) = {

    ClosureTask("EncodeIndividuals") { (context, rng, _) ⇒

      // extract inputs from context
      val entities: Array[Entity] = context(DecodeEntities.varEntities)
      val rules: Array[ClassifierRule] = context(varRules)

      System.out.println("encoding " + entities.length + " entities into " + (_characteristics.length + _actions.length) + " arrays...")

      _characteristics.zipWithIndex.foreach { case (c, i) ⇒ System.out.println(c + " => " + entities.map(e ⇒ e.characteristics(i).value).toList.toArray) }

      // forge as many outputs as expected
      val outputsForCharacteristics: List[Variable[_]] =
        _characteristics
          .zipWithIndex
          .map {
            case (c: Val[Array[T]], i) ⇒
              Variable.unsecure(
                c,
                toArrayTyped(
                  entities
                    .map(e ⇒ e.characteristics(i).value)
                )
              //.toArray(createGenericArray(c))
              )
          }
          .toList
      val outputsForActions: List[Variable[_]] =
        _actions
          .zipWithIndex
          .map {
            case (a, i) ⇒
              Variable.unsecure(
                a.prototype.toArray,
                toArrayTyped(
                  entities
                    .map(e ⇒ e.actions(i).value))
              )
          }
          .toList

      // cast the variables and return them as Arrays for each variable
      List(
        Variable(varRules, rules)
      ) ++ outputsForCharacteristics ++ outputsForActions

    } set (
      // we expect as inputs:
      inputs += (
        DecodeEntities.varEntities,
        varRules),
        // we provide as outputs
        outputs ++= _characteristics,
        outputs ++= _actions.map(g ⇒ g.prototype.toArray).toSeq,
        outputs += varRules
    )

  }
}
