/*
 * Copyright (C) 2015 Romain Reuillon
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
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.plugin.tool

import java.io.{ File, FileReader }
import java.math.{ BigDecimal, BigInteger }

import au.com.bytecode.opencsv.CSVReader
import org.openmole.core.context.{ Context, Val, Variable }
import org.openmole.core.dsl._
import org.openmole.core.exception.UserBadDataError
import org.openmole.core.workflow.builder._

import scala.annotation.tailrec

package csv {

  trait CSVPackage {
    lazy val columns = new {
      def +=[T: CSVToVariablesBuilder: InputOutputBuilder](proto: Val[_]): T ⇒ T = this.+=(proto.name, proto)
      def +=[T: CSVToVariablesBuilder: InputOutputBuilder](name: String, proto: Val[_]): T ⇒ T =
        (implicitly[CSVToVariablesBuilder[T]].columns add Mapped(proto, name)) andThen (outputs += proto)
    }

    lazy val fileColumns = new {
      def +=[T: CSVToVariablesBuilder: InputOutputBuilder](dir: File, proto: Val[File]): T ⇒ T =
        this.+=(proto.name, dir, proto)

      def +=[T: CSVToVariablesBuilder: InputOutputBuilder](name: String, dir: File, proto: Val[File]): T ⇒ T =
        (implicitly[CSVToVariablesBuilder[T]].fileColumns add (name, dir, proto)) andThen (outputs += proto)
    }
    lazy val separator = new {
      def :=[T: CSVToVariablesBuilder](s: OptionalArgument[Char]): T ⇒ T =
        implicitly[CSVToVariablesBuilder[T]].separator.set(s)

    }
  }
}

package object csv extends CSVPackage {

  import org.openmole.tool.file._
  import org.openmole.tool.stream._
  import org.openmole.core.tools.io.Prettifier._

  def writeVariablesToCSV(
    file:              File,
    prototypes:        Seq[Val[_]],
    context:           Context,
    arraysOnSingleRow: Boolean        = false,
    header:            Option[String] = None,
    overwrite:         Boolean        = false): Unit = {

    file.createParentDir

    val ps =
      if (prototypes.isEmpty) context.values.map { _.prototype }
      else prototypes

    file.withLock {
      fos ⇒

        if (overwrite) file.content = ""

        val lists = ps.map {
          p ⇒
            context.option(p) match {
              case Some(v) ⇒
                v match {
                  case v: Array[_] ⇒ v.toList
                  case l: List[_]  ⇒ l
                  case v           ⇒ List(v)
                }
              case None ⇒ List("not found")
            }
        }.toList

        if (file.size == 0)
          fos.appendLine {
            def defaultHeader =
              (ps zip lists).flatMap {
                case (p, l) ⇒
                  if (arraysOnSingleRow && moreThanOneElement(l))
                    (0 until l.size).map(i ⇒ s"${p.name}$i")
                  else List(p.name)
              }.mkString(",")

            header getOrElse defaultHeader
          }

        def moreThanOneElement(l: List[_]) = !l.isEmpty && !l.tail.isEmpty

        def flatAny(o: Any): List[Any] = o match {
          case o: List[_] ⇒ o
          case _          ⇒ List(o)
        }

        @tailrec def write(lists: List[List[_]]): Unit =
          if (lists.exists(_.isEmpty)) Unit
          else if (arraysOnSingleRow || !lists.exists(moreThanOneElement)) writeLine(lists.flatten(flatAny))
          else {
            writeLine(lists.map { _.head })
            write(lists.map { l ⇒ if (moreThanOneElement(l)) l.tail else l })
          }

        def writeLine[T](list: List[T]) = {
          fos.appendLine(list.map(l ⇒ {
            val prettified = l.prettify()
            def shouldBeQuoted = prettified.contains(',') || prettified.contains('"')
            def quote(s: String) = '"' + s.replaceAll("\"", "\"\"") + '"'
            if (shouldBeQuoted) quote(prettified) else prettified
          }).mkString(","))
        }

        write(lists)
    }
  }

  /**
   * Builds the plan.
   *
   */
  def csvToVariables(
    columns:     Vector[Mapped[_]],
    fileColumns: Vector[(String, File, Val[File])],
    separator:   Option[Char])(file: File, context: Context): Iterator[Iterable[Variable[_]]] = {
    val reader = new CSVReader(new FileReader(file), separator.getOrElse(','))
    val headers = reader.readNext.toArray

    //test wether prototype names belong to header names
    val columnsIndexes = columns.map {
      case m ⇒
        val i = headers.indexOf(m.name)
        if (i == -1) throw new UserBadDataError("Unknown column name : " + name)
        else i
    }

    val fileColumnsIndexes =
      fileColumns.map {
        case (name, _, _) ⇒
          val i = headers.indexOf(name)
          if (i == -1) throw new UserBadDataError("Unknown column name : " + name)
          else i
      }

    Iterator.continually(reader.readNext).takeWhile(_ != null).map {
      line ⇒
        (columns zip columnsIndexes).map {
          case (m, i) ⇒ Variable.unsecure(m.v, converter(m.v)(line(i)))
        } ++
          (fileColumns zip fileColumnsIndexes).map {
            case ((_, f, p), i) ⇒ Variable(p, new File(f, line(i)))
          }
    }

  }

  val conveters = Map[Class[_], (String ⇒ _)](
    classOf[BigInteger] → (new BigInteger(_: String)),
    classOf[BigDecimal] → (new BigDecimal(_: String)),
    classOf[Double] → ((_: String).toDouble),
    classOf[String] → ((_: String).toString),
    classOf[Boolean] → ((_: String).toBoolean),
    classOf[Int] → ((_: String).toInt),
    classOf[Float] → ((_: String).toFloat),
    classOf[Long] → ((_: String).toLong)
  )

  def converter[T](p: Val[_]): String ⇒ _ =
    conveters.getOrElse(p.`type`.runtimeClass, throw new UserBadDataError("Unmanaged type for csv sampling for column binded to prototype " + p))

}
