package exercises

import util.DayN

object Day9 extends DayN {
  override val num = 9

  val preamble = lines.take(25).map(_.toLong)
  val remaining = lines.drop(25).map(_.toLong)

  def firstInvalid(previous: List[Long], remaining: List[Long]): Long = remaining match {
    case h :: t => {
      val matches = for {
        x <- previous
        y <- previous
        if (x + y) == h
      } yield (x, y)

      if (matches.isEmpty) h
      else firstInvalid(h :: previous, t)
    }
  }

  def seriesSum(target: Long, remaining: List[Long]): List[Long] = {
    def loop(hist: List[Long], rem: List[Long]): List[Long] = {
      if (hist.sum == target) hist
      else if (hist.sum < target) loop(rem.head :: hist, rem.tail)
      else seriesSum(target, remaining.tail)
    }

    loop(List.empty[Long], remaining)
  }

  val fi = firstInvalid(preamble, remaining)
  part1(fi)

  val list = seriesSum(fi, preamble ++ remaining)
  part2(list.min + list.max)
}
