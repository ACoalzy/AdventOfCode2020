package exercises

import util.DayN
import util.StringUtils._

import scala.util.Try

object Day2 extends DayN {

  override val num = 2

  case class PasswordPolicy(x: Int, y: Int, char: Char)
  case class Password(value: String)

  case class PasswordWithPolicy(password: Password, policy: PasswordPolicy)

  object PasswordWithPolicy {
    def oldValidate(p: PasswordWithPolicy): Boolean = {
      val count = p.password.value.count(_ == p.policy.char)
      count >= p.policy.x && count <= p.policy.y
    }
    def newValidate(p: PasswordWithPolicy): Boolean = {
      def isCharAtPos(i: Int) = Try(p.password.value.charAt(i) == p.policy.char).getOrElse(false)
      isCharAtPos(p.policy.x -1) != isCharAtPos(p.policy.y -1)
    }
  }

  val regex = """(\d+)-(\d+) (\w): (\w+)""".r
  val policies = lines.map {
    case regex(sMin, sMax, sChar, sPassword) => PasswordWithPolicy(
      Password(sPassword),
      PasswordPolicy(sMin.toInt, sMax.toInt, sChar.head)
    )
  }

  part1(policies.count(PasswordWithPolicy.oldValidate))
  part2(policies.count(PasswordWithPolicy.newValidate))

}
