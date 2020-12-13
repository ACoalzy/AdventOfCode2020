package exercises

import util.DayN

object Day13 extends DayN {
  override val num = 13

  val start = lines.head.toLong
  val ids = lines(1).split(",").filter(_ != "x").map(_.toLong).toList

  @annotation.tailrec
  def earliestMultiple(i: Long, ints: List[Long], acc: Int): Long = {
    ints.find(i % _ == 0) match {
      case None => earliestMultiple(i + 1, ints, acc+1)
      case Some(i) => acc * i
    }
  }

  part1(earliestMultiple(start, ids, 0))

  val idOffset = lines(1).split(",").zipWithIndex.filter(_._1 != "x").map { case (s, i) => i -> s.toLong }.toList

  def lcm(list: Seq[Long]): Long = list.foldLeft(1L){
    (a, b) => b * a / LazyList.iterate((a,b)){case (x,y) => (y, x%y)}.dropWhile(_._2 != 0).head._1.abs
  }

  def earliestSequence(idOffset: List[(Int, Long)]): Long = {
    @annotation.tailrec
    def loop(l: Long): Long = {
      val matches = idOffset.filter { case (i, v) => ((l+i) % v) == 0 }
      if (matches.size == idOffset.size) l
      else loop(l + lcm(matches.map(_._2)))
    }

    val max = idOffset.maxBy(_._2)
    loop(max._2 - max._1)
  }

  part2(earliestSequence(idOffset))
}
