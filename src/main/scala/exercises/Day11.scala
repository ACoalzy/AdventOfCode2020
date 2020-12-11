package exercises

import util.DayN
import util.geometry.point.Point2D

object Day11 extends DayN {
  override val num = 11

  val floor = '.'
  val empty = 'L'
  val occ = '#'

  val seatMap: Map[Point2D, Char] = lines.map(_.zipWithIndex).zipWithIndex.flatMap {
    case (chars, y) => chars.map {
      case (c, x) => Point2D(x, y) -> c
    }
  }.toMap

  def loop(map: Map[Point2D, Char]): Map[Point2D, Char] = {
    val newMap = map.map { case (point, c) =>
      val surr = point.surrounding
      val count = surr.toList.flatMap(map.get).count(_ == occ)
        if ((c == empty) && (count == 0)) point -> occ
        else if ((c == occ) && (count >= 4)) point -> empty
        else point -> c
    }

    if (newMap == map) map
    else loop(newMap)
  }

  part1(loop(seatMap).values.count(_ == occ))
}
