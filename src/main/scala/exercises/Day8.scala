package exercises

import util.DayN
import util.StringUtils.RichStringOps

object Day8 extends DayN {

  override val num = 8

  case class Instr(action: String, value: Int)

  case class Exit(value: Long, status: Int)

  def recurse(acc: Long, nextIndex: Int, instructions: Array[Instr], history: Set[Int]): Exit = {
    if (history.contains(nextIndex)) Exit(acc, -1)
    else if (nextIndex >= instructions.length) Exit(acc, 1)
    else instructions(nextIndex) match {
      case Instr("acc", i) => recurse(acc + i, nextIndex + 1, instructions, history + nextIndex)
      case Instr("jmp", i) => recurse(acc, nextIndex + i, instructions, history + nextIndex)
      case Instr("nop", _) => recurse(acc, nextIndex + 1, instructions, history + nextIndex)
    }
  }

  def findOneChange(instructions: Array[Instr], index: Int): Long = {
    val exit = instructions(index) match {
      case Instr("jmp", i) => recurse(0, 0, instructions.updated(index, Instr("nop", i)), Set())
      case Instr("nop", i) => recurse(0, 0, instructions.updated(index, Instr("jmp", i)), Set())
      case _ => Exit(-1, -1)
    }

    if (exit.status == 1) exit.value
    else findOneChange(instructions, index + 1)
  }

  val instructions = lines.map { s =>
    val (action, valueString) = s.splitOn(' ')
    Instr(action, valueString.toInt)
  }.toArray

  part1(recurse(0, 0, instructions, Set.empty[Int]))
  part2(findOneChange(instructions, 0))

}
