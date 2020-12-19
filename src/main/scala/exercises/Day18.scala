package exercises

import util.DayN

object Day18 extends DayN {
  override val num = 18

  val mul = (a: Long, b: Long) => a * b
  val add = (a: Long, b: Long) => a + b

  def calcMaths(s: String, acc: List[Long], ops: List[(Long, Long) => Long]): Long = {
    if (s.isEmpty) acc.head
    else if (s.head.equals(')')) calcMaths(s.drop(1), ops.head(acc.head, acc.tail.head) :: acc.tail.tail, ops.tail)
    else if (s.head.equals('(')) calcMaths(s.drop(1), 1 :: acc, mul :: ops)
    else if (s.head.isDigit) {
      val numString = s.takeWhile(_.isDigit)
      calcMaths(s.drop(numString.length), ops.head(acc.head, numString.toLong) :: acc.tail, ops.tail)
    } else if (s.head == '+') calcMaths(s.drop(1), acc, add :: ops)
    else if (s.head == '*') calcMaths(s.drop(1), acc, mul :: ops)
    else calcMaths(s.drop(1), acc, ops)
  }

  part1(lines.map(s => calcMaths(s, List(1), List(mul))).sum)

  sealed trait Operation
  case object Add extends Operation
  case object Multiply extends Operation

  sealed trait Maths { def eval(priorities: List[Set[Operation]]): Long }
  case class Number(value: Long) extends Maths {
    def eval(priorities: List[Set[Operation]]): Long = value
  }

  case class Expr(nums: List[Maths], ops: List[Operation]) extends Maths {
    def eval(priorities: List[Set[Operation]]): Long = {
      priorities.foldLeft(this) { case (expr, operations) =>
        def loop(is: List[Int], numsAcc: List[Maths], opsAcc: List[Operation]): Expr = is match {
          case Nil => Expr(numsAcc, opsAcc)
          case i :: t if i == 0 => loop(t, expr.nums.head :: Nil, Nil)
          case i :: t => expr.ops(i-1) match {
            case Add if operations.contains(Add) => loop(t, Number(numsAcc.head.eval(priorities) + expr.nums(i).eval(priorities)) :: numsAcc.tail, opsAcc)
            case Multiply if operations.contains(Multiply) => loop(t, Number(numsAcc.head.eval(priorities) * expr.nums(i).eval(priorities)) :: numsAcc.tail, opsAcc)
            case _ => loop(t, expr.nums(i) :: numsAcc, expr.ops(i-1) :: opsAcc)
          }
        }

        loop(expr.nums.indices.toList, Nil, Nil)
      }
    } match {
      case Expr(nums, _) => nums.head.eval(priorities)
    }
  }

  val start = Expr(List(), List())

  def parse(s: String, acc: List[Expr]): Expr = {
    if (s.isEmpty) acc.head
    else if (s.head.equals(')')) parse(s.drop(1), Expr(acc.tail.head.nums :+ acc.head, acc.tail.head.ops) :: acc.tail.tail)
    else if (s.head.equals('(')) parse(s.drop(1), start :: acc)
    else if (s.head.isDigit) {
      val numString = s.takeWhile(_.isDigit)
      parse(s.drop(numString.length), Expr(acc.head.nums :+ Number(numString.toLong), acc.head.ops) :: acc.tail)
    } else if (s.head == '+') parse(s.drop(1), Expr(acc.head.nums, acc.head.ops :+ Add) :: acc.tail)
    else if (s.head == '*') parse(s.drop(1), Expr(acc.head.nums, acc.head.ops :+ Multiply) :: acc.tail)
    else parse(s.drop(1), acc)
  }

  part1(lines.map(s => parse(s, List(start)).eval(List(Set(Add, Multiply)))).sum)
  part2(lines.map(s => parse(s, List(start)).eval(List(Set(Add), Set(Multiply)))).sum)

}
