package scala.lab04

import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.lab04.SetADTs.{BasicSetADT, SetADT}

abstract class SetADTCheck(name: String) extends Properties(name):
  val setADT: SetADT
  import setADT.*

  // generating a small Int
  def smallInt(): Gen[Int] = Gen.choose(0, 10)
  // generating a Set of Int with approximate size (modulo clashes)
  def setGen[A: Arbitrary](size: Int): Gen[Set[A]] =
    if size == 0
      then Gen.const(empty())
    else for
      a <- Arbitrary.arbitrary[A]
      s <- setGen(size - 1)
    yield s.add(a)
  // a given instance to generate sets with small size
  given arb: Arbitrary[Set[Int]] = Arbitrary:
    for
      i <- smallInt()
      s <- setGen[Int](i)
    yield s

  property("commutativity of union") =
    forAll: (s1: Set[Int], s2: Set[Int]) =>
      (s1 || s2) === (s2 || s1)

  /**
    * axioms defining contains based on empty/add:
    * contains(empty, x) = false
    * contains(add(x,s), y) = (x == y) || contains(s, y)
  */

  property("axioms for contains") =
     forAll: (s: Set[Int], x: Int, y:Int) =>
        s.add(x).contains(y) == (x == y) || s.contains(y)
   &&
     forAll: (x: Int) =>
        !empty().contains(x)

/**
 * axioms defining union and remove:
 * union(empty, s) = s
 * union(add(x, s2), s) = add(x, union(s2, s)
 * remove(x, empty) = empty
 * remove(x, add(x, s)) = remove(x, s)
 * remove(x, add(y, s)) = add(y, remove(x, s)) if x!=y
 *
 * and so on: write axioms and correspondingly implement checks
 */

  property("axioms for union") =
    forAll: (s: Set[Int]) =>
      s.union(empty()) == s
    &&
      forAll: (s1: Set[Int], s2: Set[Int], x: Int) =>
        s2.add(x).union(s1) == s2.union(s1).add(x)

  property("axioms for remove") =
    forAll: (x: Int) =>
      empty().remove(x) == empty()
    &&
      forAll: (x: Int, s: Set[Int]) =>
        s.add(x).remove(x) == s.remove(x)
    &&
      forAll: (x: Int, y: Int, s: Set[Int]) =>
        x != y ==> (s.add(y).remove(x) == s.remove(x).add(y))

/**
 * add
 *  add(x, add(x, s)) = add(x, s)
 *  add(x, add(y, s)) = add(y, add(x, s)) if x!=y
 */

  property("axioms for add") =
    forAll: (x: Int, s: Set[Int]) =>
      s.add(x).add(x) == s.add(x)
    &&
      forAll: (x: Int, y: Int, s: Set[Int]) =>
        s.add(y).add(x) === s.add(x).add(y)

/**
 * axioms for intersection:
 * intersection(empty, s) = empty
 * intersection(s, s) = s
 * contains(intersection(s1, s2), x) = contains(s1, x) && contains(s2, x)
 */

  property("axioms for intersection") =
    forAll: (s: Set[Int]) =>
      s.intersection(empty()) == empty()
    &&
      forAll: (s: Set[Int]) =>
        s.intersection(s) == s
    &&
      forAll: (s1: Set[Int], s2: Set[Int], x: Int) =>
        s1.intersection(s2).contains(x) == (s1.contains(x) && s2.contains(x))


/**
 *  l'unione è distributiva rispetto all'intersezione
 */
  property("distributive property of union over intersection") =
    forAll: (s1: Set[Int], s2: Set[Int], s3: Set[Int]) =>
      s1.union(s2.intersection(s3)) === (s1.union(s2)).intersection(s1.union(s3))


/**
 * axioms for size:
 * size(empty) = 0
 * size(add(x, s)) = if contains(s, x) then size(s) else 1 + size(s)
 *
 * size(union(s1, s2)) <= size(s1) + size(s2)
 * size(intersection(s1, s2)) <= min(size(s1), size(s2))
 * size(diff(s1, s2)) <= size(s1)
 */

  property("axioms for size") =
    forAll: () =>
      empty().size() == 0
    &&
    forAll: (s: Set[Int]) =>
      s.size() >= 0
    && forAll: (s: Set[Int], x: Int) =>
      s.add(x).size() == (if s.contains(x) then s.size() else 1 + s.size())
    && forAll: (s1: Set[Int], s2: Set[Int]) =>
      s1.union(s2).size() <= s1.size() + s2.size()
    && forAll: (s1: Set[Int], s2: Set[Int]) =>
      s1.intersection(s2).size() <= math.min(s1.size(), s2.size())

object BasicSetADTCheck extends SetADTCheck("SequenceBased Set"):
  val setADT: SetADT = BasicSetADT

  @main def visuallingCheckArbitrarySets =
    Range(0,20).foreach(i => println(summon[Arbitrary[setADT.Set[Int]]].arbitrary.sample))
