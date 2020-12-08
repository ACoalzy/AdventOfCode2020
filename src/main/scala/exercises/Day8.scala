package exercises

import instructions._
import util.DayN

object Day8 extends DayN {

  override val num = 8

  @annotation.tailrec
  def fixCorruptInstr(instructions: Vector[Instruction], index: Int): Long = {
    val exit = instructions(index) match {
      case Instruction(Jmp, i) => Instructions.run(instructions.updated(index, Instruction(Nop, i)))
      case Instruction(Nop, i) => Instructions.run(instructions.updated(index, Instruction(Jmp, i)))
      case _ => Result(-1, Failure)
    }

    if (exit.status == Success) exit.value
    else fixCorruptInstr(instructions, index + 1)
  }

  val instructions = lines.map(Instruction.fromString).toVector

  part1(Instructions.run(instructions))
  part2(fixCorruptInstr(instructions, 0))

}
