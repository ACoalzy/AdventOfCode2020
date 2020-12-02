package util

object StringUtils {

  implicit class RichStringOps(private val s: String) extends AnyVal {
    def splitOn(char: Char): (String, String) = {
      val (a, b) = s.splitAt(s.indexOf(char))
      (a, b.drop(1))
    }
  }

}
