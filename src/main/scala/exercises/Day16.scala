package exercises

import util.DayN
import util.StringUtils.RichStringOps

object Day16 extends DayN {
  override val num = 16

  def toRange(s: String): Range = {
    val (left, right) = s.splitOn('-')
    left.toInt to right.toInt
  }

  case class Rule(field: String, ranges: List[Range]) {
    def contains(i: Int): Boolean = ranges.exists(_.contains(i))
  }

  object Rule {
    val RuleRegex = """(.+): (.+) or (.+)""".r
    def fromString(s: String): Rule = s match {
      case RuleRegex(name, range1, range2) =>Rule(name, List(toRange(range1), toRange(range2)))
    }
  }

  lines.mkString("\n").split("\n\n").toList match {
    case ruleStrings :: myticket :: otherticketStrings :: Nil =>
      val rules = ruleStrings.split("\n").map(Rule.fromString)

      part1 {
        otherticketStrings
          .split("\n").drop(1)
          .flatMap(_.split(",").map(_.toInt))
          .filterNot(i => rules.exists(_.ranges.exists(_.contains(i))))
          .sum
      }

      part2 {
        val validTickets = otherticketStrings
          .split("\n").drop(1)
          .map(_.split(",").map(_.toInt))
          .filter(is => is.forall(i => rules.exists(_.ranges.exists(_.contains(i)))))

        val map: Map[Rule, Seq[Int]] = rules.indices.flatMap(i => {
          val vs = validTickets.map(_(i))
          rules.filter(r => vs.forall(r.contains)).map(r => (r, i))
        }).groupMap(_._1)(_._2)

        @annotation.tailrec
        def loop(map: Map[Rule, Seq[Int]]): Map[Rule, Seq[Int]] = {
          if (map.values.forall(_.size == 1)) map
          else {
            val doneIs = map.keys.filter(k => map(k).size == 1).flatMap(map).toSet
            loop(map.map { case (k, v) => if (v.size > 1) k -> v.filterNot(doneIs.contains) else k -> v})
          }
        }

        val ticket = myticket.split("\n").drop(1).head.split(",").map(_.toLong)
        loop(map).filter(_._1.field.startsWith("departure")).flatMap(_._2).map(ticket).product
      }
  }
}
