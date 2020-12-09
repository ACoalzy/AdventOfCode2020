package exercises

import util.DayN
import util.IterableUtils.RichIOps

object Day9 extends DayN {
  override val num = 9

  def invalidPreamble(target: Long, preamble: List[Long]): Boolean =
    !preamble.indices.combinations(2).exists(_.map(preamble.apply).sum == target)

  def firstInvalid(preambleSize: Int, list: List[Long]): Long = list.indices.drop(preambleSize)
    .find(i => invalidPreamble(list(i), list.slice(i - preambleSize, i)))
    .map(i => list(i)).get

  def seriesSumsToTarget(target: Long, list: List[Long]): Option[List[Long]] = list.indices.to(LazyList)
    .map(i => i -> list.take(i+1).sum).tuples
    .flatMap {
      case ((a, sumA), (b, sumB)) => Option.when((sumB - sumA) == target)(list.slice(a, b))
    }.headOption

  val nums = lines.map(_.toLong)
  val firstInvalidNumber = firstInvalid(25, nums)
  part1(firstInvalidNumber)

  val series = seriesSumsToTarget(firstInvalidNumber, nums).get
  part2(series.min + series.max)
}
