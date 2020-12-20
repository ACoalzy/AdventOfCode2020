package exercises

import util.DayN

object Day19 extends DayN {
  override val num = 19

  val test = """0: 4 1 5
               |1: 2 3 | 3 2
               |2: 4 4 | 5 5
               |3: 4 5 | 5 4
               |4: "a"
               |5: "b"
               |
               |ababbb
               |bababa
               |abbbab
               |aaabbb
               |aaaabbb""".stripMargin.split("\n").toList

  sealed trait Rule
  case class Options(options: List[List[Int]]) extends Rule
  case class Value(value: Char) extends Rule

  val ruleRegex = """(\d+): (.*)""".r
  val valueRegex = """"(.+)"""".r

  def parse(lines: List[String]): Map[Int, Rule] = lines.map(_ match {
    case ruleRegex(iString, optionsString) =>
      optionsString match {
        case valueRegex(v) => iString.toInt -> Value(v.head)
        case _ => iString.toInt -> Options(optionsString.split(" \\| ").map(_.split(" ").map(_.toInt).toList).toList)
      }
  }).toMap

  def validateString(s: String, rules: Map[Int, Rule]): Boolean = {
    def loop(s: String, rule: Rule): List[Int] = {
      if (s.isEmpty) List()
      else rule match {
        case Value(c) if s.headOption.contains(c) => List(1)
        case Options(opts) => opts.flatMap(_.map(rules).foldLeft(List(0)) { case (sizes, r) =>
          sizes.flatMap(i => loop(s.drop(i), r).map(_ + i))
        })
        case _ => List()
      }
    }

    loop(s, rules(0)).contains(s.length)
  }

  val rulesLines = lines.mkString("\n").split("\n\n").head.split("\n").toList
  val messages = lines.mkString("\n").split("\n\n")(1).split("\n").toList
  val ruleMap = parse(rulesLines)

  part1(messages.count(s => validateString(s, ruleMap)))

  val alteredRules = ruleMap
    .updated(8, Options(List(List(42), List(42, 8))))
    .updated(11, Options(List(List(42, 31), List(42, 11, 31))))

  part2(messages.count(s => validateString(s, alteredRules)))

}
