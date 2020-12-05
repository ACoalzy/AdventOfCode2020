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

  part1(binaryStrings.sorted.takeRight(1).map(s => Integer.parseInt(s, 2)))

}