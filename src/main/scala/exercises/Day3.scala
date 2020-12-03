package exercises

import util.DayN
import util.geometry.point.Point2D

object Day3 extends DayN {
  override val num = 3

  def treesInSlope(grid: List[List[Char]], maxX: Int, maxY: Int)(slope: Point2D): Long = {
    @annotation.tailrec
    def loop(current: Point2D, count: Long): Long = {
      val next = Point2D((current.x + slope.x) % maxX, current.y + slope.y)
      val newCount = if (grid(current.y)(current.x) == '#') count + 1 else count

      if (next.y > maxY) newCount
      else loop(next, newCount)
    }

    loop(Point2D(0, 0), 0)
  }

  val parsed = lines.map(_.toList)
  val maxX = lines.head.length
  val maxY = lines.size - 1

  part1(treesInSlope(parsed, maxX, maxY)(Point2D(3, 1)))

  val slopes = List(Point2D(1, 1), Point2D(3, 1), Point2D(5, 1), Point2D(7, 1), Point2D(1, 2))
  part2(slopes.map(treesInSlope(parsed, maxX, maxY)).product)

}
