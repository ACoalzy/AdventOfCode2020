package exercises

import util.DayN
import util.geometry.point.Point2D

object Day11 extends DayN {
  override val num = 11

  val floor = '.'
  val empty = 'L'
  val occ = '#'

  val seatMap: Map[Point2D, Char] = Point2D.toCharMap(lines)

  def adjacent(point: Point2D, map: Map[Point2D, Char])(move: Point2D): Option[Char] = map.get(point + move)

  @annotation.tailrec
  def firstVisible(point: Point2D, map: Map[Point2D, Char])(move: Point2D): Option[Char] = map.get(point + move) match {
    case None => None
    case Some(c) if c != floor => Some(c)
    case _ => firstVisible(point + move, map)(move)
  }

  @annotation.tailrec
  def loop(map: Map[Point2D, Char], threshold: Int)(f: (Point2D, Map[Point2D, Char]) => Point2D => Option[Char]): Map[Point2D, Char] = {
    val newMap = map.map { case (point, c) =>
      val count = point.surroundingDirs.toList.flatMap(f(point, map)).count(_ == occ)
      if ((c == empty) && (count == 0)) point -> occ
      else if ((c == occ) && (count >= threshold)) point -> empty
      else point -> c
    }

    if (newMap == map) map else loop(newMap, threshold)(f)
  }

  part1(loop(seatMap, 4)(adjacent).values.count(_ == occ))

  part2(loop(seatMap, 5)(firstVisible).values.count(_ == occ))
}
