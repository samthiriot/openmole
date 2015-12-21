package org.openmole.gui.client.core

/*
 * Copyright (C) 07/10/15 // mathieu.leclaire@openmole.org
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

import org.openmole.gui.client.core.files._
import org.openmole.gui.ext.data._
import org.openmole.gui.misc.js.{ Select, OMTags }
import autowire._
import org.scalajs.dom.html.TextArea
import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
import org.openmole.gui.client.core.files.treenodemanager.{ instance ⇒ manager }
import org.scalajs.dom.raw.{ HTMLDivElement, HTMLInputElement }
import org.openmole.gui.misc.js.JsRxTags._
import rx._
import org.openmole.gui.shared.Api
import scalatags.JsDom.{ TypedTag, tags ⇒ tags }

import scalatags.JsDom.all._
import fr.iscpif.scaladget.api.{ BootstrapTags ⇒ bs, ClassKeyAggregator }
import bs._

class ModelWizardPanel extends ModalPanel {
  lazy val modalID = "modelWizardPanelID"

  def onOpen() = {}

  def onClose() = {}

  sealed trait VariableRole[T] {
    def content: T

    def clone(t: T): VariableRole[T]

    def switch: VariableRole[T]
  }

  case class Input[T](content: T) extends VariableRole[T] {
    def clone(otherT: T) = Input(otherT)

    def switch = Output(content)
  }

  case class Output[T](content: T) extends VariableRole[T] {
    def clone(otherT: T): VariableRole[T] = Output(otherT)

    def switch = Input(content)
  }

  case class CommandInput[T](content: T) extends VariableRole[T] {
    def clone(otherT: T) = CommandInput(otherT)

    def switch = CommandOutput(content)
  }

  case class CommandOutput[T](content: T) extends VariableRole[T] {
    def clone(otherT: T): VariableRole[T] = CommandOutput(otherT)

    def switch = CommandInput(content)
  }

  implicit def pairToLine(variableElement: VariableRole[VariableElement]): Reactive = buildReactive(variableElement)

  implicit def stringToOptionString(s: String): Option[String] = if (s.isEmpty) None else Some(s)

  val transferring: Var[FileTransferState] = Var(Standby())
  val labelName: Var[Option[String]] = Var(None)
  val launchingCommand: Var[Option[LaunchingCommand]] = Var(None)
  val currentReactives: Var[Seq[Reactive]] = Var(Seq())
  val updatableTable: Var[Boolean] = Var(true)
  val hasModel: Var[Boolean] = Var(false)
  val bodyContent: Var[Option[TypedTag[HTMLDivElement]]] = Var(None)
  val resources: Var[Seq[TreeNodeData]] = Var(Seq())
  val currentTab: Var[Int] = Var(0)
  val autoMode = Var(true)
  val upButton: Var[HTMLDivElement] = Var(tags.div().render)
  // val filters: Var[Seq[ClassTree]] = Var(Seq())
  // Usefull for jar namespaces
  val javaLikeLanguage = Var(false)
  val classSelector: Select[FullClass] = Select("classSelector", Seq[(FullClass, ClassKeyAggregator)](), None, btn_default)

  val commandArea: TextArea = bs.textArea(3)("").render
  val autoModeCheckBox = bs.checkbox(autoMode())(onchange := { () ⇒
    autoMode() = !autoMode()
  })

  val scriptNameInput = bs.input("", "modelNameInput")(placeholder := "Script name").render
  val codeSelector: Select[Language] = Select("selectLanguages",
    Seq(Binary(), JavaLikeLanguage(), PythonLanguage(), NetLogoLanguage(), RLanguage()).map {
      (_, emptyCK)
    }, Some(Binary()),
    btn_default
  )

  Obs(launchingCommand, skipInitial = true) {
    if (autoMode()) {
      commandArea.value = launchingCommand().map {
        _.fullCommand
      }.getOrElse("")
    }
  }

  Obs(currentReactives, skipInitial = true) {
    if (updatableTable()) setBodyContent
  }

  def buttonStyle(i: Int): ClassKeyAggregator = {
    if (i == currentTab()) btn_primary
    else btn_default
  } + "marginRight20"

  def nbInputs = inputs(currentReactives()).size

  def nbOutputs = currentReactives().size - nbInputs

  def inputs(reactives: Seq[Reactive]): Seq[VariableRole[VariableElement]] = {
    reactives.map {
      _.role
    }.collect {
      case x: Input[VariableElement]        ⇒ x
      case x: CommandInput[VariableElement] ⇒ x
    }
  }

  def outputs(reactives: Seq[Reactive]): Seq[VariableRole[VariableElement]] =
    reactives.map {
      _.role
    }.collect {
      case x: Output[VariableElement]        ⇒ x
      case x: CommandOutput[VariableElement] ⇒ x
    }

  def getReactive(index: Int): Option[Reactive] = currentReactives().filter {
    _.index == index
  }.headOption

  def setUpButton = upButton() =
    bs.div("centerWidth250")(
      tags.label(`class` := "inputFileStyle spacer5 certificate")(
        bs.fileInput((fInput: HTMLInputElement) ⇒ {
          FileManager.upload(fInput,
            manager.current.safePath(),
            (p: FileTransferState) ⇒ {
              transferring() = p
            },
            UploadProject(),
            () ⇒ {
              if (fInput.files.length > 0) {
                val fileName = fInput.files.item(0).name
                OMPost[Api].launchingCommands(manager.current.safePath() ++ fileName).call().foreach { b ⇒
                  panels.treeNodePanel.refreshCurrentDirectory
                  launchingCommand() = b.headOption
                  hasModel() = true
                  labelName() = Some(fileName)
                  launchingCommand().foreach { lc ⇒
                    codeSelector.content() = lc.language
                    scriptNameInput.value = fileName.split('.').head
                    lc.language match {
                      case Some(j: JavaLikeLanguage) ⇒ OMPost[Api].classes(manager.current.safePath() ++ fileName).call().foreach { b ⇒
                        javaLikeLanguage() = true
                        classSelector.setContents(b.flatMap {
                          _.flatten
                        }.map {
                          (_, emptyCK)
                        })
                      }
                      case _ ⇒ javaLikeLanguage() = false
                    }

                    val nbArgs = lc.arguments.size
                    val iReactives = lc.arguments.zipWithIndex.collect {
                      case (ve: VariableElement, id: Int) ⇒ buildReactive(CommandInput(ve), id)
                    }
                    val oReactives = lc.outputs.zipWithIndex collect {
                      case (ve: VariableElement, id: Int) ⇒ buildReactive(CommandOutput(ve), id + nbArgs)
                    }
                    currentReactives() = iReactives ++ oReactives
                  }
                }
                OMPost[Api].listFiles(manager.current).call().foreach { lf ⇒
                  resources() = lf
                }
              }
            }
          )
        }), labelName() match {
          case Some(s: String) ⇒ s
          case _               ⇒ "Your Model"
        }
      ), {
        if (javaLikeLanguage()) bs.span("grey")(classSelector.selectorWithFilter)
        else tags.div()
      }, {
        if (hasModel()) bs.span("right grey")(codeSelector.selector)
        else tags.div()
      }
    ).render

  val step1 = tags.div(
    tags.h4("Step 1: Code import"),
    bs.div("grey")("Pick your code up among jar archive, netlogo scripts, or any code packaged on linux with Care ( like Python, C, C++ " +
      "R, etc). In the case of a Care archive, the packaging has to be done with the",
      tags.b(" -o yourmodel.tar.gz.bin."),
      " option."
    )
  )

  val step2 = tags.div(
    bs.div("grey")("The systems detects automatically the launching command and propose you the creation of some OpenMOLE Variables so that" +
      " your model will be able to be feeded with variable values coming from the workflow you will build afterwards. In the case of Java, Scala, Netlogo" +
      "(ie codes working on the JVM) the OpenMOLE variables can be set directly in the command line. Otherwise, they have to be set inside ${} statements." +
      " By default he systems detects automatically your Variable changes and update the launching command. However, this option can be desactivated."
    )
  )

  val autoModeTag = bs.div("onecolumn spacer20")(
    tags.b("Launching Command"),
    bs.div("spacer4 right")(
      "Automatic ",
      autoModeCheckBox,
      bs.span("grey")(" It is automatically updated (default), or it can be set manually")
    )
  )

  val buildModelTaskButton = {
    bs.button(
      "Build",
      btn_primary)(onclick := {
        () ⇒
          save
          close
          launchingCommand().foreach {
            lc ⇒
              OMPost[Api].buildModelTask(
                labelName().getOrElse(""),
                scriptNameInput.value,
                commandArea.value,
                codeSelector.content().getOrElse(Binary()),
                inputs(currentReactives()).map {
                  _.content.prototype
                },
                outputs(currentReactives()).map {
                  _.content.prototype
                },
                manager.current.safePath()).call().foreach {
                  b ⇒
                    panels.treeNodePanel.refreshCurrentDirectory
                  // panels.treeNodePanel.fileDisplayer
                }
          }
      })
  }

  def save = {
    currentReactives().map {
      _.save
    }
  }

  def buildReactive(role: VariableRole[VariableElement], index: Int): Reactive = Reactive(role, index)

  def buildReactive(role: VariableRole[VariableElement]): Reactive =
    currentReactives().filter {
      _.role == role
    }.headOption.getOrElse(buildReactive(role, role.content.index))

  def addVariableElement(p: VariableRole[VariableElement]) = {
    save
    currentReactives() = currentReactives() :+ buildReactive(p, -1)
  }

  def applyOnPrototypePair(p: VariableRole[VariableElement], todo: (VariableRole[VariableElement], Int) ⇒ Unit) =
    currentReactives().map {
      _.role
    }.zipWithIndex.filter {
      case (ptp, index) ⇒ ptp == p
    }.foreach {
      case (role, index) ⇒ todo(role, index)
    }

  def updatePrototypePair(p: VariableRole[VariableElement], variableElement: VariableElement) =
    applyOnPrototypePair(p, (role: VariableRole[VariableElement], index: Int) ⇒ currentReactives() = currentReactives().updated(index, buildReactive(role.clone(variableElement), index)))

  def switchPrototypePair(p: VariableRole[VariableElement]) = {
    save
    applyOnPrototypePair(p, (role: VariableRole[VariableElement], index: Int) ⇒ currentReactives() = currentReactives().updated(index, buildReactive(role.switch, index)))
  }

  def addSwitchedPrototypePair(p: VariableRole[VariableElement]) = {
    save
    currentReactives() = (currentReactives() :+ buildReactive(p.switch, -1)) distinct
  }

  case class Reactive(role: VariableRole[VariableElement], index: Int) {
    val lineHovered: Var[Boolean] = Var(false)

    val switchGlyph = role match {
      case i: Input[_]         ⇒ OMTags.glyph_arrow_right
      case ci: CommandInput[_] ⇒ OMTags.glyph_arrow_right
      case _                   ⇒ OMTags.glyph_arrow_left
    }

    def updateLaunchingCommand =
      role match {
        case CommandInput(_) | CommandOutput(_) ⇒
          launchingCommand() = launchingCommand().map { lc ⇒
            val statics = lc.statics
            lc.copy(arguments = statics ++
              currentReactives().map {
                _.role
              }.collect {
                case x: CommandInput[_]  ⇒ x
                case y: CommandOutput[_] ⇒ y
              }.map {
                _.content
              }
            )
          }
        case _ ⇒
      }

    def save = getReactive(index).map { reactive ⇒ updatePrototypePair(reactive.role, reactive.role.content.clone(nameInput.value, typeSelector.content().get, mappingInput.value)) }

    def removePrototypePair = {
      currentReactives() = currentReactives().filterNot(_.role == role)
      ModelWizardPanel.this.save
    }

    lazy val typeSelector: Select[ProtoTYPE.ProtoTYPE] = Select("modelProtos",
      ProtoTYPE.ALL.map {
        (_, emptyCK)
      }, Some(role.content.prototype.`type`),
      btn_primary, onclickExtra = () ⇒ {
        save
      }
    )

    def saveWithoutTableUpdate = {
      updatableTable() = false
      save
      updatableTable() = true
    }

    val mappingInput: HTMLInputElement = bs.input(role.content.prototype.mapping.getOrElse(""))(oninput := { () ⇒
      saveWithoutTableUpdate
    }).render

    val nameInput: HTMLInputElement = bs.input(role.content.prototype.name)(oninput := { () ⇒
      saveWithoutTableUpdate
      updateLaunchingCommand
    }
    ).render

    val line = {
      typeSelector.content() = Some(role.content.prototype.`type`)
      tags.tr(
        onmouseover := { () ⇒
          lineHovered() = true
        },
        onmouseout := { () ⇒
          lineHovered() = false
        },
        bs.td(bs.col_md_3 + "spacer7")(nameInput),
        bs.td(bs.col_md_2)(typeSelector.selector),
        bs.td(bs.col_md_1 + "grey")(role.content.prototype.default),
        bs.td(bs.col_md_3)(if (role.content.prototype.mapping.isDefined) mappingInput else tags.div()),
        bs.td(bs.col_md_1 + "right")(
          id := Rx {
            "treeline" + {
              if (lineHovered()) "-hover" else ""
            }
          }, glyphSpan(switchGlyph, () ⇒ switchPrototypePair(role))(id := "glyphtrash", `class` := "glyphitem grey spacer2"),
          glyphSpan(OMTags.glyph_arrow_right_and_left, () ⇒ addSwitchedPrototypePair(role))(id := "glyphtrash", `class` := "glyphitem grey spacer2")
        ), bs.td(bs.col_md_1 + "right")(
          id := Rx {
            "treeline" + {
              if (lineHovered()) "-hover" else ""
            }
          },
          glyphSpan(glyph_trash, () ⇒ removePrototypePair)(id := "glyphtrash", `class` := "glyphitem grey spacer2")
        )
      )
    }
  }

  def setBodyContent: Unit = bodyContent() = Some({
    val reactives = currentReactives()
    val topButtons = bs.div("spacer20")(
      Rx {
        bs.badge("I/O", s"$nbInputs/$nbOutputs",
          buttonStyle(0)
        )(onclick := {
            () ⇒
              currentTab() = 0
              setBodyContent
          })
      }, Rx {
        bs.badge("Resources", s"${
          resources().size
        }", buttonStyle(1))(onclick := {
          () ⇒
            currentTab() = 1
            setBodyContent
        })
      }
    )
    setUpButton

    tags.div(
      hasModel() match {
        case true ⇒ tags.div()
        case _    ⇒ step1
      },
      transferring() match {
        case _: Transfering ⇒ OMTags.waitingSpan(" Uploading ...", btn_danger + "certificate")
        case _: Transfered  ⇒ upButton()
        case _              ⇒ upButton()
      },
      hasModel() match {
        case true ⇒
          bs.div("spacer80")(
            tags.h4("Step2: Task configuration"), step2,
            topButtons,
            if (currentTab() == 0) {
              tags.div({

                val iinput: HTMLInputElement = bs.input("")(placeholder := "Add Input").render

                val oinput: HTMLInputElement = bs.input("")(placeholder := "Add Output").render

                val head = thead(tags.tr(
                  for (h ← Seq("Name", "Type", "Default", "Mapped with", "", "")) yield {
                    tags.th(h)
                  }))

                bs.div("spacer50")(
                  bs.div("twocolumns right10")(
                    bs.form("paddingLeftRight50")(iinput,
                      onsubmit := {
                        () ⇒
                          addVariableElement(Input(VariableElement(-1, ProtoTypePair(iinput.value, ProtoTYPE.DOUBLE), CareTaskType())))
                          iinput.value = ""
                          false
                      }),
                    bs.table(striped)(
                      head,
                      tbody(
                        for (ip ← inputs(reactives)) yield {
                          ip.line
                        }))),
                  tags.div(`class` := "twocolumns")(
                    bs.form("paddingLeftRight50")(oinput, onsubmit := {
                      () ⇒
                        addVariableElement(Output(VariableElement(-1, ProtoTypePair(oinput.value, ProtoTYPE.DOUBLE), CareTaskType())))
                        oinput.value = ""
                        false
                    }),
                    bs.table(striped)(
                      head,
                      tbody(
                        for (op ← outputs(reactives)) yield {
                          op.line
                        }
                      )
                    )
                  )
                )
              }, autoModeTag, commandArea)
            }
            else {
              tags.div("resources")
            }
          )
        case _ ⇒ tags.div()
      }
    )
  })

  lazy val dialog = {
    setBodyContent
    bs.modalDialog(modalID,
      headerDialog(
        tags.span(tags.b("Model import"))
      ),
      bodyDialog(Rx {
        bodyContent().getOrElse(tags.div())
      }),
      footerDialog(OMTags.buttonGroup("width200Right100")(
        inputGroupButton(closeButton),
        inputGroupButton(scriptNameInput),
        inputGroupButton(buildModelTaskButton)
      )
      )
    )
  }

}