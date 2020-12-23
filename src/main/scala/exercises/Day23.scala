package exercises

import util.{DayN, Timer}

object Day23 extends DayN {
  override val num = 23

  object mutableNightmare {
    import scala.collection.mutable

    class Node[A](val value: A, var prev: Node[A], var next: Node[A])

    object Node {
      def apply[A](value: A, prev: => Node[A], next: => Node[A]) = new Node[A](value, prev, next)
    }

    class DoubleLinkedList[A] private(a: A) {
      private var start: Node[A] = Node(a, start, start)
      private var end: Node[A] = start
      private val map: mutable.Map[A, Node[A]] = mutable.Map(a -> start)

      def getStart: Node[A] = start

      def append(a: A): Unit = {
        val node = Node(a, end, start)
        map.update(a, node)
        end.next = node
        end = node
      }

      def insert(n: Node[A], a: A): Unit = {
        val node = Node(a, n, n.next)
        map.update(a, node)
        n.next = node
        node.next.prev = node
      }

      def remove(n: Node[A]): Unit = {
        n.prev.next = n.next
        n.next.prev = n.prev
        map.remove(n.value)
        if (n == start) start = start.next
        if (n == end) end = end.prev
      }

      def find(a: A): Option[Node[A]] = map.get(a)

      def toList: List[A] = {
        def loop(curr: Node[A], hist: Set[Node[A]], acc: List[A]): List[A] = {
          if (hist.contains(curr)) acc.reverse
          else loop(curr.next, hist + curr, curr.value :: acc)
        }
        loop(start, Set(), List())
      }

      override def toString: String = toList.mkString(",")
    }

    object DoubleLinkedList {
      def from[A](head: A, tail: List[A]): DoubleLinkedList[A] = {
        val list = new DoubleLinkedList[A](head)
        tail.foreach(a => list.append(a))
        list
      }
    }
  }

  def newCard(i: Int, d: Int, m: Int): Int = {
    val res = i - d
    if (res <= 0) m + res else res
  }

  def crabCups(ints: List[Int], count: Long): List[Int] = {
    import mutableNightmare._

    val cups: DoubleLinkedList[Int] = DoubleLinkedList.from(ints.head, ints.tail)

    val lazyRange = (1 to 4).to(LazyList)
    val size = ints.size

    @annotation.tailrec
    def loop(current: Node[Int], count: Long): Unit = if (count == 0) () else {
      val taken = List(current.next, current.next.next, current.next.next.next)
      taken.foreach(cups.remove)

      val destination = lazyRange.flatMap(i => cups.find(newCard(current.value, i, size))).head
      taken.foldLeft(destination) { case (d, n) => cups.insert(d, n.value); d.next }

      loop(current.next, count - 1)
    }

    loop(cups.getStart, count)
    cups.toList
  }

  val input = lines.head.map(_.asDigit)

  part1 {
    val result = crabCups(input.toList, 100)
    val (left, right) = result.splitAt(result.indexOf(1))
    (right.tail ::: left).mkString
  }

  part2 {
    val result = crabCups(input.toList ::: (10 to 1000000).toList, 10000000L)
    result.drop(result.indexOf(1) + 1).take(2).map(_.toLong).product
  }
}
