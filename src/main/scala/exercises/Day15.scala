package exercises

import util.{DayN, Timer}

object Day15 extends DayN {
  override val num = 15

  val test: List[Long] = List(0,3,6)
  val start: List[Long] = lines.head.split(",").map(_.toLong).toList

  @annotation.tailrec
  def loop(hist: List[Long], count: Long): List[Long] =
    if (count == 0) hist
    else loop(hist.tail.indexOf(hist.head) + 1 :: hist, count - 1)

  part1(loop(start.reverse, 2020 - start.size).head)

  val startMap: Map[Long, Long] = start.dropRight(1).zipWithIndex.map { case (v, i) => v -> i.toLong }.toMap

  def slower(init: List[Long], target: Long): Long = {
    @annotation.tailrec
    def loop(hist: Map[Long, Long], value: Long, index: Long, target: Long): Map[Long, Long] =
      if (index == target) hist
      else {
        val newValue = index - hist.getOrElse(value, index)
        loop(hist.updated(value, index), newValue, index + 1, target)
      }

    val startMap: Map[Long, Long] = init.dropRight(1).zipWithIndex.map { case (v, i) => v -> i.toLong }.toMap
    loop(startMap, init.last, init.size -1, target).maxBy(_._2)._1
  }

  def faster(init: List[Long], target: Long): Long = {
    val startMap: Map[Long, Long] = init.dropRight(1).zipWithIndex.map { case (v, i) => v -> i.toLong }.toMap
    val hist: scala.collection.mutable.Map[Long, Long] = startMap.to(collection.mutable.Map)

    @annotation.tailrec
    def loop(value: Long, index: Long, target: Long): Long =
      if (index == target) hist.maxBy(_._2)._1
      else {
        val newValue = index - hist.getOrElse(value, index)
        hist(value) = index
        loop(newValue, index + 1, target)
      }

    loop(init.last, init.size -1, target)
  }

  Timer.time(part2(slower(start, 30000000)))
  Timer.time(part2(faster(start, 30000000)))
}
