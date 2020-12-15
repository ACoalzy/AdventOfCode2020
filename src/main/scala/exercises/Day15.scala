package exercises

import util.DayN

object Day15 extends DayN {
  override val num = 15

  val test: List[Long] = List(0,3,6)
  val start: List[Long] = lines.head.split(",").map(_.toLong).toList

  def loop(hist: List[Long], count: Long): List[Long] =
    if (count == 0) hist
    else loop(hist.tail.indexOf(hist.head) + 1 :: hist, count - 1)

  part1(loop(start.reverse, 2020 - start.size).head)

  val startMap: Map[Long, Long] = start.dropRight(1).zipWithIndex.map { case (v, i) => v -> i.toLong }.toMap

  def loop2(hist: Map[Long, Long], value: Long, index: Long, target: Long): Map[Long, Long] =
    if (index == target) hist
    else {
      val newValue = index - hist.getOrElse(value, index)
      loop2(hist.updated(value, index), newValue, index + 1, target)
    }

  part2(loop2(startMap, start.last, start.size -1, 30000000).maxBy(_._2)._1)
}
