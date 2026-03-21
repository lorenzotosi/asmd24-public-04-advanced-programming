package scala.lab04

import u04.datastructures.Sequences.*
import Sequence.*

import Trees.*
import Trees.Tree.*
import Trees.Tree.{add as treeAdd, contains as treeContains}
import scala.annotation.tailrec

object SetADTs:
  
  trait SetADT:
    type Set[A]
    def empty[A](): Set[A]
    extension [A](s: Set[A])
      def add(element: A): Set[A]
      def contains(a: A): Boolean
      def union(other: Set[A]): Set[A]
      def intersection(other: Set[A]): Set[A]
      infix def ||(other: Set[A]): Set[A] = s.union(other)
      infix def &&(other: Set[A]): Set[A] = s.intersection(other)
      def remove(a: A): Set[A]
      def toSequence(): Sequence[A]
      def size(): Int
      def ===(other: Set[A]): Boolean
    

  object BasicSetADT extends SetADT:

    opaque type Set[A] = Sequence[A]

    def empty[A](): Set[A] = Nil()

    extension [A](s: Set[A])
      def add(element: A): Set[A] = s match
        case Cons(h, _) if h == element => s
        case Cons(h, t)  => Cons(h, t.add(element))
        case _ => Cons(element, Nil())

      def remove(a: A): Set[A] = s.filter(_ != a)  

      def contains(a: A): Boolean = s match
        case Cons(h, t) => h == a || t.contains(a)
        case Nil() => false

      def toSequence(): Sequence[A] = s

      def union(s2: Set[A]): Set[A] = s2 match
        case Cons(h, t) => Cons(h, s.remove(h).union(t))
        case Nil() => s

      def intersection(s2: Set[A]): Set[A] = s match
        case Cons(h, t) if s2.contains(h) => Cons(h, t.intersection(s2.remove(h)))
        case Cons(_, t) => t.intersection(s2)
        case Nil() => Nil()

      def size(): Int = s match
        case Cons(_, t) => 1 + t.size()
        case Nil() => 0

      def ===(other: Set[A]): Boolean =
        s.union(other).size() == s.size()

  object TreeBasedSetADT extends SetADT:

    opaque type Set[A] = Tree[A]

    private given hashOrdering[A]: Ordering[A] with
      def compare(x: A, y: A): Int =
        if x == y then 0
        else if x.hashCode < y.hashCode then -1
        else 1

    override def empty[A](): Set[A] = Empty()

    extension [A](s: Tree[A])

      def add(element: A): Set[A] = treeAdd(s)(element)(using hashOrdering)

      override def contains(a: A): Boolean = treeContains(s)(a)(using hashOrdering)

      override def union(other: Tree[A]): Set[A] =
        def loop(t: Tree[A], acc: Set[A]): Set[A] = t match
          case Empty() => acc
          case Node(v, l, r) => loop(r, loop(l, acc.add(v)))
        loop(other, s)


      override def intersection(other: Set[A]): Set[A] = ???

      override def remove(a: A): Set[A] = s match
          case Empty() => Empty()
          case Node(v, l, r) =>
            val cmp = hashOrdering.compare(a, v)
            if cmp == 0 then
              if l == Empty() then r
              else if r == Empty() then l
              else
                @tailrec
                def findMin(t: Tree[A]): A = t match
                  case Node(minV, Empty(), _) => minV
                  case Node(_, left, _) => findMin(left)
                  case Empty() => throw new NoSuchElementException()

                val minRight = findMin(r)
                Node(minRight, l, r.remove(minRight))
            else if cmp < 0 then Node(v, l.remove(a), r)
            else Node(v, l, r.remove(a))

      override def toSequence(): Sequence[A] = ???

      override def size(): Int = s match
        case Empty() => 0
        case Node(_, l, r) => 1 + l.size() + r.size()

      override def ===(other: Set[A]): Boolean = ???

@main def trySetADTModule =
  import SetADTs.*
  val setADT: SetADT = BasicSetADT
  import setADT.*

  val s1: Set[Int] = empty().add(10).add(20).add(30)
  val s2: Set[Int] = empty().add(10).add(11)
  // val s3: Set[Int] = Cons(10, Nil()) // because Set is defined opaque
  println(s1.toSequence()) // (10, 20, 30)
  println(s2.toSequence()) // (10, 11)
  println(s1.union(s2).toSequence()) // (10, 20, 30, 11)
  println(s1.intersection(s2).toSequence()) // (10)