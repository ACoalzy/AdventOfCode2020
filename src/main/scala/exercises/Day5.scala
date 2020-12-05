package exercises

import util.DayN

object Day5 extends DayN {
  override val num = 5

  val binaryStrings = lines.map(_
    .replace('F', '0')
    .replace('L', '0')
    .replace('B', '1')
    .replace('R', '1')
  )
  val seatNumbers = binaryStrings.map(s => Integer.parseInt(s, 2)).sorted

  part1(seatNumbers.last)

  def seriesSum(length: Long): Long = (length * (length + 1)) / 2

  part2(seriesSum(seatNumbers.last) - seatNumbers.sum - seriesSum(seatNumbers.head -1))

}