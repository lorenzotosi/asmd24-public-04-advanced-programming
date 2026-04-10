package u04.monads

import Monads.*, Monad.*, States.*, State.*
import u04.datastructures.Streams.*

trait WindowState:
  type Window
  def initialWindow: Window
  def setSize(width: Int, height: Int): State[Window, Unit]
  def addButton(text: String, name: String): State[Window, Unit]
  def addTextBox(text: String, name: String): State[Window, Unit]
  def fromTextBox(name: String): State[Window, String]
  def addLabel(text: String, name: String): State[Window, Unit]
  def toLabel(text: String, name: String): State[Window, Unit]
  def show(): State[Window, Unit]
  def exec(cmd: =>Unit): State[Window, Unit]
  def eventStream(): State[Window, Stream[String]]

object WindowStateImpl extends WindowState:
  import SwingFunctionalFacade.*
  
  type Window = Frame
  
  
  def initialWindow: Window = createFrame

  def setSize(width: Int, height: Int): State[Window, Unit] = 
    State(w => ((w.setSize(width, height)), {}))
  def addButton(text: String, name: String): State[Window, Unit] =
    State(w => ((w.addButton(text, name)), {}))
  def addLabel(text: String, name: String): State[Window, Unit] =
    State(w => ((w.addLabel(text, name)), {}))
  def toLabel(text: String, name: String): State[Window, Unit] =
    State(w => ((w.showToLabel(text, name)), {}))
  def show(): State[Window, Unit] =
    State(w => (w.show, {}))
  def exec(cmd: =>Unit): State[Window, Unit] =
    State(w => (w, cmd))  
  def eventStream(): State[Window, Stream[String]] =
    State(w => (w, Stream.generate(() => w.events().get)))
  def addTextBox(text: String, name: String): State[Window, Unit] =
    State(w => ((w.addTextBox(text, name)), {}))
  def fromTextBox(name: String): State[Window, String] =
    State(w => (w, w.textOf(name)))

@main def windowStateExample =
  import u04.*
  import WindowStateImpl.*
  import u04.datastructures.Streams.*

  val windowCreation = for 
    _ <- setSize(300, 300)
    _ <- addButton(text = "inc", name = "IncButton")
    _ <- addButton(text = "dec", name = "DecButton")
    _ <- addButton(text = "quit", name = "QuitButton")
    _ <- addTextBox(text = "0", name = "Input1")
    _ <- addLabel(text = "-", name = "Label1")
    _ <- show()
    e <- eventStream()
  yield e

  val windowEventsHandling = for
    _ <- windowCreation
    e <- eventStream()
    _ <- seqN(e.map(_ match
        case "IncButton" => toLabel("i", "Label1")
        case "DecButton" => toLabel("d", "Label1")
        case "QuitButton" => exec(sys.exit())))
  yield ()

  windowEventsHandling.run(initialWindow)