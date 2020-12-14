package exercises

import util.DayN

object Day14 extends DayN {
  override val num = 14

  val maskRegex = """mask = (.+)""".r
  val memRegex = """mem\[(\d+)\] = (\d+)""".r

  def padLeft(s: String, length: Int, c: Char): String =
    (LazyList.continually(c).take(length).mkString + s).drop(s.length)

  def toBinaryString(num: Long, length: Int): String =
    padLeft(num.toBinaryString, length, '0')

  def parseLong(radix: Int)(s: String): Long = java.lang.Long.parseLong(s, radix)

  @annotation.tailrec
  def loop(rem: List[String], mask: String, mem: Map[Long, Long]): Map[Long, Long] = rem match {
    case Nil => mem
    case h :: t => h match {
      case maskRegex(mask) => loop(t, mask, mem)
      case memRegex(addr, num) => {
        val newString = mask.zip(toBinaryString(num.toLong, 36))
          .map { case (m, i) => if (m == 'X') i else m }.mkString
        loop(t, mask, mem.updated(addr.toInt, parseLong(2)(newString)))
      }
    }
  }

  val result = loop(lines, "", Map.empty)
  part1(result.values.sum)

  @annotation.tailrec
  def mask2(s: String, m: String, ss: List[String]): List[String] = {
    if (m.isEmpty) ss
    else if (m.startsWith("1")) mask2(s.tail, m.tail, ss.map(_.appended('1')))
    else if (m.startsWith("0")) mask2(s.tail, m.tail, ss.map(_.appended(s.head)))
    else mask2(s.tail, m.tail, ss.map(_.appended('1')) ++ ss.map(_.appended('0')))
  }

  @annotation.tailrec
  def loop2(rem: List[String], mask: String, mem: Map[Long, Long]): Map[Long, Long] = rem match {
    case Nil => mem
    case h :: t => h match {
      case maskRegex(mask) => loop2(t, mask, mem)
      case memRegex(addr, num) => {
        val newMem = mask2(toBinaryString(addr.toLong, 36), mask, List("")).map(parseLong(2))
          .foldLeft(mem) { case (m, v) => m.updated(v, num.toLong) }
        loop2(t, mask, newMem)
      }
    }
  }

  val result2 = loop2(lines, "", Map.empty)
  part2(result2.values.sum)

}
