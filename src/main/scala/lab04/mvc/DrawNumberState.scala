package scala.lab04.mvc

import u04.monads.{Monads, States}
import u04.monads.States.State

trait DrawNumberState:
  type DrawNumber

  enum GuessResult:
    case Correct
    case GameOver(n: Int)
    case TooHigh(left: Int)
    case TooLow(left: Int)

  def initialDrawNumber(target: Int, maxAttempts: Int): DrawNumber
  def guess(n: Int): State[DrawNumber, GuessResult]
  def get(): State[DrawNumber, Int]
  def reset(target: Int, attempts: Int): State[DrawNumber, Unit]
  def nop(): State[DrawNumber, Unit]

object DrawNumberStateImpl extends DrawNumberState:
  import GuessResult.*

  opaque type DrawNumber = (Int, Int)

  def initialDrawNumber(target: Int, maxAttempts: Int): DrawNumber = (target, maxAttempts)

  def guess(n: Int): State[DrawNumber, GuessResult] = State:
    case (target, atts) =>
      val remaining = atts - 1
      if n == target then
        ((target, 0), Correct)
      else if remaining <= 0 then
        ((target, 0), GameOver(target))
      else if n < target then
        ((target, remaining), TooLow(remaining))
      else
        ((target, remaining), TooHigh(remaining))
  
  def get(): State[DrawNumber, Int] = State:
    case state @ (target, atts) => (state, atts)

  def reset(target: Int, attempts: Int): State[DrawNumber, Unit] = State:
    _ => ((target, attempts), ())

  def nop(): State[DrawNumber, Unit] = State:
    state => (state, ())

@main def tryDrawNumberState =
  import Monads.*, Monad.*, States.{*, given}, State.*
  val game: DrawNumberState = DrawNumberStateImpl
  import game.*

  // Inizializziamo il gioco con target 50 e 3 tentativi massimi
  val initialState = initialDrawNumber(50, 3)

  // 1. Singolo tentativo
  println("--- Singolo tentativo ---")
  println:
    guess(20).run(initialState)
  // Output: ((50,2), TooLow(2)) -> (NuovoStato, Risultato)

  // 2. Sequenza base
  println("\n--- Sequenza di tentativi ---")
  println:
    seq(guess(20), guess(80)).run(initialState)
  // Output: ((50,1), TooHigh(1)) -> Mostra l'ultimo risultato e i tentativi scalati a 1

  // 3. Sessione complessa tramite For-Comprehension
  val session: State[DrawNumber, (GuessResult, Int)] =
    for
      res1 <- guess(20)             // TooLow
      _    <- guess(80)             // TooHigh
      left <- get()                 // Legge i tentativi rimasti (1)
      _    <- reset(10, 5)               // Resetta tutto: nuovo target 10, 5 tentativi
      res3 <- guess(10)             // Correct
      finalLeft <- get()            // Legge i tentativi rimasti (4)
    yield (res3, finalLeft)

  println("\n--- Sessione monadica completa ---")
  println:
    session.run(initialState)
// Output: ((10,4), (Correct,4)) -> Lo stato finale e il risultato restituito dalla yield