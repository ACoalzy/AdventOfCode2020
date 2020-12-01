package exercises

import util.DayN

object Day1 extends DayN {
  override val num = 1

  def findPairSum(ints: List[Int], target: Int): Option[(Int, Int)] = {
    val sumPairs = for {
      a <- ints
      b <- ints
    } yield (a+b) -> (a, b)

    sumPairs.toMap.get(target)
  }

  def findTripleSum(ints: List[Int], target: Int): Option[(Int, Int, Int)] = {
    val sumPairs = for {
      a <- ints
      b <- ints
      c <- ints
    } yield (a+b+c) -> (a, b, c)

    sumPairs.toMap.get(target)
  }

  val parsed = lines.map(_.toInt)
  part1(findPairSum(parsed, 2020).map { case (a, b) => a*b })
  part2(findTripleSum(parsed, 2020).map { case (a, b, c) => a*b*c })
}
