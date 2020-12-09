package util

object IterableUtils {

  implicit class RichIOps[A](private val iterable: Iterable[A]) extends AnyVal {
    def tuples: Iterable[(A, A)] = {
      val withI = iterable.zipWithIndex
      for {
        a <- withI
        b <- withI
        if a._2 != b._2
      } yield (a._1, b._1)
    }
  }

}
