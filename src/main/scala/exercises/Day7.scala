package exercises

import util.DayN

object Day7 extends DayN {
  override val num = 7

  val keyValueRegex = """(.*) bags contain (.*)\.""".r
  val valueRegex = """(\d+) (\w+ \w+) bags?""".r

  case class BagRule(name: String, amount: Int) {
    def *(mul: Int): BagRule = BagRule(name, amount * mul)
  }

  val ruleMap: Map[String, List[BagRule]] = lines.map {
    case keyValueRegex(key, valueString) =>
      val rules = valueRegex.findAllMatchIn(valueString).map(_.subgroups match {
        case amount :: name :: Nil => BagRule(name, amount.toInt)
      }).toList
      key -> rules
  }.toMap

  @annotation.tailrec
  def recurseUp(colours: Set[String]): Set[String] = {
    val newColours = ruleMap.filter(_._2.map(_.name).exists(colours)).keys.toSet
    if (newColours.size == colours.size) colours
    else recurseUp(newColours ++ colours)
  }

  @annotation.tailrec
  def countBags(queue: List[BagRule], count: Int): Int = queue match {
    case Nil => count
    case h :: t => countBags(ruleMap(h.name).map(_ * h.amount) ::: t, count + h.amount)
  }

  part1(recurseUp(Set("shiny gold")).size)
  part2(countBags(ruleMap("shiny gold"), 0))
}
