package exercises

import util.DayN

object Day4 extends DayN {
  override val num = 4

  val passportStrings = lines.mkString("\n").split("\n\n")
  val passportFieldStrings = passportStrings.map(_.split("""\s"""))

  part1(passportFieldStrings.count(array => (array.length == 8) || ((array.length == 7) && (!array.exists(_.startsWith("cid"))))))


}
