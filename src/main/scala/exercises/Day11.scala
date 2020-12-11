package exercises

import util.DayN
import util.geometry.point.Point2D

object Day11 extends DayN {
  override val num = 11

  val floor = '.'
  val empty = 'L'
  val occ = '#'

  val seatMap: Map[Point2D, Char] = Point2D.toCharMap(lines)

  @annotation.tailrec
  def loop(map: Map[Point2D, Char]): Map[Point2D, Char] = {
    val newMap = map.map { case (point, c) =>
      val count = point.surrounding.toList.flatMap(map.get).count(_ == occ)
      if ((c == empty) && (count == 0)) point -> occ
      else if ((c == occ) && (count >= 4)) point -> empty
      else point -> c
    }

    if (newMap == map) map else loop(newMap)
  }

  part1(loop(seatMap).values.count(_ == occ))

  def firstVisible(start: Point2D, map: Map[Point2D, Char])(move: Point2D): Option[Char] = {
    val result = map.get(start + move)
    if (result.exists(_ != floor)) result
    else if (result.isDefined) firstVisible(start + move, map)(move)
    else None
  }

  @annotation.tailrec
  def loop2(map: Map[Point2D, Char]): Map[Point2D, Char] = {
    val newMap = map.map { case (point, c) =>
      val count = point.surroundingDirs.toList.flatMap(firstVisible(point, map)).count(_ == occ)
      if ((c == empty) && (count == 0)) point -> occ
      else if ((c == occ) && (count >= 5)) point -> empty
      else point -> c
    }

    if (newMap == map) map else loop2(newMap)
  }

  part2(loop2(seatMap).values.count(_ == occ))
}
