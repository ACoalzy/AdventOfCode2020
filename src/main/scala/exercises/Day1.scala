package exercises

import util.DayN

object Day1 extends DayN {
  override val num = 1

  def findPairSum(ints: List[Int], target: Int): Option[(Int, Int)] = {
    val set = ints.toSet
    set.find(i => set.contains(target-i))
      .map(i => (i, target - i))
  }

  def findTripleSum(ints: List[Int], target: Int): Option[(Int, Int, Int)] = {
    ints.toStream
      .flatMap(i => findPairSum(ints, target - i).map { case (a, b) => (a, b, i) })
      .headOption
  }

  val parsed = lines.map(_.toInt)
  part1(findPairSum(parsed, 2020).map { case (a, b) => a*b })
  part2(findTripleSum(parsed, 2020).map { case (a, b, c) => a*b*c })
}
