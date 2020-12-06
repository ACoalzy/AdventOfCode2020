package exercises

import util.DayN

object Day6 extends DayN {
  override val num = 6

  val groups = lines.mkString("\n").split("\n\n")
  val sets = groups.map(_.toSet)
  val answers = ('a' to 'z').toSet

  part1(sets.map(_.intersect(answers).size).sum)

  val people = groups.map(_.split("\n").map(_.toSet))
  part2(people.map(arr => arr.fold(answers)(_.intersect(_)).size).sum)
}
