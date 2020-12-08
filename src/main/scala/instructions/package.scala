import util.StringUtils.RichStringOps

package object instructions {

  sealed trait ExitStatus
  case object Success extends ExitStatus
  case object Failure extends ExitStatus

  case class Result(value: Long, status: ExitStatus)

  sealed trait InstructionType
  case object Acc extends InstructionType
  case object Jmp extends InstructionType
  case object Nop extends InstructionType

  object InstructionType {
    def fromString(s: String): InstructionType = s match {
      case "jmp" => Jmp
      case "acc" => Acc
      case "nop" => Nop
    }
  }

  case class Instruction(itype: InstructionType, value: Int)

  object Instruction {
    def fromString(s: String): Instruction = {
      val (action, valueString) = s.splitOn(' ')
      Instruction(InstructionType.fromString(action), valueString.toInt)
    }
  }

  object Instructions {

    def run(instructions: Vector[Instruction]): Result = {
      @annotation.tailrec
      def loop(acc: Long, nextIndex: Int, history: Set[Int]): Result = {
        if (history.contains(nextIndex)) Result(acc, Failure)
        else if (nextIndex >= instructions.length) Result(acc, Success)
        else instructions(nextIndex) match {
          case Instruction(Acc, i) => loop(acc + i, nextIndex + 1, history + nextIndex)
          case Instruction(Jmp, i) => loop(acc, nextIndex + i, history + nextIndex)
          case Instruction(Nop, _) => loop(acc, nextIndex + 1, history + nextIndex)
        }
      }

      loop(0, 0, Set.empty[Int])
    }

  }
}
