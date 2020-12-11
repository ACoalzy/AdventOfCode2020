package exercises

import util.DayN

object Day10 extends DayN {
  override val num = 10

  val b = lines.map(_.toInt)
  val ad: List[Int] = (0 :: (b.max + 3) :: b).sorted

  val diffs = ad.sliding(2).map { case a :: b :: Nil => b - a }.toList

  part1(diffs.count(_ == 1) * diffs.count(_ == 3))

  val jolts = 1 to 3

  @annotation.tailrec
  def loop(rem: List[Int], hist: Map[Int, Long]): Long = rem match {
    case h :: t => loop(t, hist + (h -> jolts.flatMap(j => hist.get(h - j)).sum))
    case Nil => hist(hist.keys.max)
  }

  part2(loop(ad.drop(1), Map(0 -> 1L)))
}
