package scala.lab04.mvc

@main def runMVC =
  import u04.monads.Monads.*, Monad.*, u04.monads.States.*, State.*, u04.monads.WindowStateImpl.*
  import u04.datastructures.Streams.*
  import DrawNumberStateImpl.*
  import GuessResult.*

  val attempts = 5

  def getRandomNumber: Int =
    scala.util.Random.nextInt(50)

  def mv[SM, SV, AM, AV](m1: State[SM,AM], f: AM => State[SV,AV]): State[(SM,SV), AV] =
    State:
      case (sm, sv) =>
        val (sm2, am) = m1.run(sm)
        val (sv2, av) = f(am).run(sv)
        ((sm2, sv2), av)

  def windowCreation(str: String): State[Window, Stream[String]] = for
    _ <- setSize(300, 300)
    _ <- addButton(text = "Guess", name = "GuessButton")
    _ <- addButton(text = "reset", name = "ResetButton")
    _ <- addButton(text = "quit", name = "QuitButton")
    _ <- addTextBox(text = "", name = "Input1")
    _ <- addLabel(text = str, name = "Label1")
    _ <- addLabel(text = "Fai un tentativo!", name = "ResultLabel")
    _ <- show()
    events <- eventStream()
  yield events

  def inputNumber(name: String): State[Window, Int] =
    fromTextBox(name).map(_.toIntOption.getOrElse(0))

  val controller = for
    events <- mv(seq(nop(), get()), i => windowCreation(s"Tentativi: $i"))
    _ <- seqN(events.map:
      case "GuessButton" =>
        for
          n <- mv(nop(), _ => inputNumber("Input1"))
          _ <- mv(guess(n), res => toLabel(res match
            case Correct     => "Indovinato"
            case TooHigh(_)  => "Troppo alto"
            case TooLow(_)   => "Troppo basso"
            case GameOver(n)    => "Partita conclusa. Premi Reset. Il numero era " + n
            , "ResultLabel"))
          _ <- mv(get(), atts => toLabel(s"Tentativi: $atts", "Label1"))
        yield ()
      case "ResetButton" =>
        for
          _ <- mv(seq(reset(getRandomNumber, attempts), get()), atts => toLabel(s"Tentativi: $atts", "Label1"))
          _ <- mv(nop(), _ => toLabel("Nuova partita!", "ResultLabel"))
        yield ()
      case "QuitButton" =>
        mv(nop(), _ => exec(sys.exit()))
    )
  yield ()

  controller.run((initialDrawNumber(getRandomNumber, attempts), initialWindow))