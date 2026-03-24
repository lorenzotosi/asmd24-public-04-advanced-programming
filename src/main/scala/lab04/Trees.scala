package scala.lab04

import scala.annotation.tailrec

object Trees:

  enum Tree[+A]:
    case Empty()
    case Node(value: A, left: Tree[A], right: Tree[A])

  object Tree:

    def of[A](elems: A*)(using Ordering[A]): Tree[A] =
      elems.foldLeft(Empty[A]())((tree, elem) => tree.add(elem))

    extension [A](t: Tree[A])

      def contains(elem: A)(using ord: Ordering[A]): Boolean =
        @tailrec
        def search(tree: Tree[A]): Boolean = tree match
          case Empty() => false
          case Node(v, left, right) =>
            val cmp = ord.compare(elem, v)
            if cmp < 0 then search(left)
            else if cmp > 0 then search(right)
            else true
        search(t)

      def add(elem: A)(using ord: Ordering[A]): Tree[A] = t match
        case Empty() => Node(elem, Empty(), Empty())
        case Node(v, left, right) =>
          val cmp = ord.compare(elem, v)
          if cmp < 0 then Node(v, left.add(elem), right)
          else if cmp > 0 then Node(v, left, right.add(elem))
          else t

      def remove(elem: A)(using ord: Ordering[A]): Tree[A] = t match
        case Empty() => Empty()
        case Node(v, l, r) =>
          val cmp = ord.compare(elem, v)
          if cmp == 0 then
            if l == Empty() then r
            else if r == Empty() then l
            else
              @tailrec
              def findMin(tree: Tree[A]): A = tree match
                case Node(minV, Empty(), _) => minV
                case Node(_, left, _) => findMin(left)
                case _ => throw new NoSuchElementException()

              val minRight = findMin(r)
              Node(minRight, l, r.remove(minRight))
          else if cmp < 0 then Node(v, l.remove(elem), r)
          else Node(v, l, r.remove(elem))


@main def tryTrees() =
  import Trees.*
  import Tree.*

  val myTree = Tree.of(10, 20, 5, 10, 30)

  println(myTree.contains(20))
  println(myTree.contains(99))

  val biggerTree = myTree.add(15).add(40)
  println(biggerTree.contains(15))