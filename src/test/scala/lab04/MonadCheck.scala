package scala.lab04

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import u04.monads.Monads.Monad
import u04.monads.Optionals.Optional.*
import u04.monads.Optionals.{Optional, given}
import u04.monads.States.State



abstract class MonadCheck[M[_]](name: String) extends Properties(name):
  val monad: Monad[M]
  import monad.*

  def smallInt(): Gen[Int] = Gen.choose(0, 10)

  def optionalGen[A: Arbitrary]: Gen[Optional[A]] =
    for
      a <- Arbitrary.arbitrary[A]
      b <- Gen.oneOf(Gen.const(true), Gen.const(false))
    yield if b then Just(a) else Empty()

  given arbMInt: Arbitrary[M[Int]]
  given arbMIntFun: Arbitrary[Int => M[Int]]

  extension [A](m1: M[A])
    def === (m2: M[A]): Prop =
      Prop(m1 == m2)

// monadic laws:
// Left identity: flatMap(unit(a),f) === f(a)
// Right identity: flatMap(m,unit(_)) === m
// Associativity: flatMap(flatMap(m,f),g) ===
//                flatMap(m,x=>flatMap(f(x),g))

  property("left identity") =
    forAll: (a: Int, f: Int => M[Int]) =>
      unit(a).flatMap(f) === f(a)

  property("right identity") =
    forAll: (m: M[Int]) =>
      m.flatMap(unit) === m

  property("associativity") =
    forAll: (m: M[Int], f: Int => M[Int], g: Int => M[Int]) =>
      m.flatMap(f).flatMap(g) ===
        m.flatMap(x => f(x).flatMap(g))

object MyMonadCheck extends MonadCheck[Optional]("Monad check"):
  val monad: Monad[Optional] = summon[Monad[Optional]]

  given arbMInt: Arbitrary[Optional[Int]] = Arbitrary(optionalGen[Int])

  given arbMIntFun: Arbitrary[Int => Optional[Int]] = Arbitrary:
    Gen.oneOf[Int => Optional[Int]](
      x => Just(x),
      x => Just(x + 1),
      x => Just(-x),
      _ => Empty()
    )

type StateInt[S] = State[Int, S]

object StateMonadCheck extends MonadCheck[StateInt]("State monad check"):
  val monad: Monad[StateInt] = summon[Monad[StateInt]]

  given arbMInt: Arbitrary[StateInt[Int]] = Arbitrary:
    for
      f <- Arbitrary.arbitrary[Int => (Int, Int)]
    yield State(f)

  given arbMIntFun: Arbitrary[Int => StateInt[Int]] = Arbitrary:
    for
      f <- Arbitrary.arbitrary[Int => Int => (Int, Int)]
    yield (x: Int) => State(f(x))
