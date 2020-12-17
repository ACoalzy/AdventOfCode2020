package exercises

import util.{DayN, Timer}
import util.geometry.point.{Point2D, Point3D}

object Day17 extends DayN {
  override val num = 17

  def cross[A](as: List[List[A]], bs: Seq[A]): List[List[A]] = for {
    a <- as
    b <- bs
  } yield b :: a

  def womboCombo[A](N: Int, f: Int => Seq[A]) = (0 until N)
    .map(f)
    .foldLeft(List(Nil): List[List[A]]){ case (acc, r) => cross(acc, r) }
    .map(_.reverse)

  def simulate(map: Map[List[Int], Char], N: Int, counter: Int): Map[List[Int], Char] = if (counter == 0) map else {
    def newRange(f: List[Int] => Int) = (map.keys.map(f).min - 1) to (map.keys.map(f).max + 1)

    val options = womboCombo(N, i => newRange(_(i)))
    val newMap = options.map { p =>
      val c = map.getOrElse(p, '.')
      val surroundingOptions = womboCombo(N, _ => -1 to 1).filterNot(_ == List.fill(N)(0)).map(_.zip(p).map { case (a, b) => a + b })
      val surrounding = surroundingOptions.flatMap(n => map.get(n))
      (c, surrounding.count(_ == '#')) match {
        case ('#', 2) | ('#', 3) => p -> '#'
        case ('#', _) => p -> '.'
        case ('.', 3) => p -> '#'
        case ('.', _) => p -> '.'
      }
    }.toMap
    simulate(newMap, N, counter - 1)
  }

  val map = Point2D.toCharMap(lines)
  part1(simulate(map.map { case (p, c) => List(p.x, p.y, 0) -> c}, 3, 6).count(_._2 == '#'))
  part2(simulate(map.map { case (p, c) => List(p.x, p.y, 0, 0) -> c}, 4, 6).count(_._2 == '#'))
}
