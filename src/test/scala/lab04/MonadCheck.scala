package scala.lab04

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import u04.monads.Monads.Monad
import u04.monads.Optionals.Optional.*
import u04.monads.Optionals.{Optional, given}
import u04.monads.States.State

import scala.util.{Success, Try, Failure}

abstract class MonadCheck[M[_], A: Arbitrary](name: String) extends Properties(name):
  val monad: Monad[M]
  import monad.*

  given arbitraryMonad: Arbitrary[M[A]]
  given arbitraryBindFunc: Arbitrary[A => M[A]]

  def checkEquality[I](m1: M[I], m2: M[I]): Prop =
    Prop(m1 == m2)

  extension [I](m1: M[I])
    infix def === (m2: M[I]): Prop =
      checkEquality(m1, m2)

  property("left identity") =
    forAll: (a: A, f: A => M[A]) =>
      unit(a).flatMap(f) === f(a)

  property("right identity") =
    forAll: (m: M[A]) =>
      m.flatMap(unit) === m

  property("associativity") =
    forAll: (m: M[A], f: A => M[A], g: A => M[A]) =>
      m.flatMap(f).flatMap(g) === m.flatMap(x => f(x).flatMap(g))

object MyMonadCheck extends MonadCheck[Optional, Int]("Monad check"):
  val monad: Monad[Optional] = summon[Monad[Optional]]

  def optionalGen[T: Arbitrary]: Gen[Optional[T]] =
    Arbitrary.arbitrary[Option[T]].map:
      case Some(v) => Just(v)
      case None    => Empty()

  given arbitraryMonad: Arbitrary[Optional[Int]] = Arbitrary(optionalGen[Int])

  given arbitraryBindFunc: Arbitrary[Int => Optional[Int]] = Arbitrary:
    Gen.oneOf[Int => Optional[Int]](
      x => Just(x),
      x => Just(x + 1),
      x => Just(-x),
      _ => Empty()
    )

type StateInt[A] = State[Int, A]

object StateMonadCheck extends MonadCheck[StateInt, Int]("State Monad check"):
  val monad: Monad[StateInt] = summon[Monad[StateInt]]

  def stateGen[A: Arbitrary]: Gen[StateInt[A]] =
    Arbitrary.arbitrary[Int => (Int, A)].map: f =>
      State(f)

  given arbitraryMonad: Arbitrary[StateInt[Int]] = Arbitrary(stateGen[Int])

  given arbitraryBindFunc: Arbitrary[Int => StateInt[Int]] = Arbitrary:
    Gen.oneOf[Int => StateInt[Int]](
      x => State(s => (x, s + 1)),
      x => State(s => (x + 1, s)),
      x => State(s => (-x, s * 2)),
      _ => State(s => (0, 0))
    )

  override def checkEquality[I](m1: StateInt[I], m2: StateInt[I]): Prop =
    forAll: (initialState: Int) =>
      m1.run(initialState) == m2.run(initialState)

type EitherString[A] = Either[String, A]

given eitherMonad: Monad[EitherString] with
  def unit[A](value: A): EitherString[A] = Right(value)
  extension [A](m: EitherString[A])
    def flatMap[B](f: A => EitherString[B]): EitherString[B] = m match
      case Right(v) => f(v)
      case Left(e)  => Left(e)

object EitherMonadCheck extends MonadCheck[EitherString, Int]("Either Monad check"):
  val monad: Monad[EitherString] = summon[Monad[EitherString]]

  def eitherGen[A: Arbitrary]: Gen[EitherString[A]] =
    for
      isRight <- Gen.oneOf(true, false)
      value   <- Arbitrary.arbitrary[A]
      error   <- Gen.alphaNumStr
    yield if isRight then Right(value) else Left(error)

  given arbitraryMonad: Arbitrary[EitherString[Int]] = Arbitrary(eitherGen[Int])

  given arbitraryBindFunc: Arbitrary[Int => EitherString[Int]] = Arbitrary:
    Gen.oneOf[Int => EitherString[Int]](
      x => Right(x),
      x => Right(x + 1),
      x => Right(-x),
      _ => Left("Error")
    )

given tryMonad: Monad[Try] with
  def unit[A](value: A): Try[A] = Success(value)
  extension [A](m: Try[A])
    def flatMap[B](f: A => Try[B]): Try[B] = m.flatMap(f)

object TryMonadCheck extends MonadCheck[Try, Int]("Try Monad check"):
  val monad: Monad[Try] = summon[Monad[Try]]

  override def checkEquality[I](m1: Try[I], m2: Try[I]): Prop = (m1, m2) match
    case (Success(v1), Success(v2)) =>
      Prop(v1 == v2)
    case (Failure(e1), Failure(e2)) =>
      Prop(e1.getClass == e2.getClass && e1.getMessage == e2.getMessage)
    case _ =>
      Prop(false)

  def tryGen[A: Arbitrary]: Gen[Try[A]] =
    for
      isSuccess <- Gen.oneOf(true, false)
      value     <- Arbitrary.arbitrary[A]
    yield if isSuccess then Success(value) else Failure(RuntimeException("Fallimento di test"))

  given arbitraryMonad: Arbitrary[Try[Int]] = Arbitrary(tryGen[Int])

  given arbitraryBindFunc: Arbitrary[Int => Try[Int]] = Arbitrary:
    Gen.oneOf[Int => Try[Int]](
      x => Success(x * 2),
      x => Failure(RuntimeException("Fallimento di test")),
      x => Success(0)
    )
