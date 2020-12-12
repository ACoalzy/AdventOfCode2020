package exercises

import util.DayN
import util.geometry.direction._
import util.geometry.point.Point2D

object Day12 extends DayN {
  override val num = 12

  case class Instr(c: Char, a: Int)

  val instrs = lines.map(s => Instr(s.head, s.tail.toInt))

  @annotation.tailrec
  def loop(rem: List[Instr], location: Point2D, facing: Point2D): Point2D = rem match {
    case Nil => location
    case h :: t => h match {
      case Instr('F', a) => loop(t, location + (facing * a), facing)
      case Instr('N', a) => loop(t, location + (Up.mutation * a), facing)
      case Instr('E', a) => loop(t, location + (Right.mutation * a), facing)
      case Instr('S', a) => loop(t, location + (Down.mutation * a), facing)
      case Instr('W', a) => loop(t, location + (Left.mutation * a), facing)
      case Instr('L', a) => loop(t, location, 0.until(a / 90).foldLeft(facing){ case (f, _) => f.rotate(AntiClockwise) })
      case Instr('R', a) => loop(t, location, 0.until(a / 90).foldLeft(facing){ case (f, _) => f.rotate(Clockwise) })
      case _ => loop(t, location, facing)
    }
  }

  part1(loop(instrs, Point2D(0, 0), Right.mutation).manHattanDist(Point2D(0, 0)))

  @annotation.tailrec
  def loop2(rem: List[Instr], location: Point2D, waypoint: Point2D): Point2D = rem match {
    case Nil => location
    case h :: t => h match {
      case Instr('F', a) => loop2(t, location + (waypoint * a), waypoint)
      case Instr('N', a) => loop2(t, location, waypoint + (Up.mutation * a))
      case Instr('E', a) => loop2(t, location, waypoint + (Right.mutation * a))
      case Instr('S', a) => loop2(t, location, waypoint + (Down.mutation * a))
      case Instr('W', a) => loop2(t, location, waypoint + (Left.mutation * a))
      case Instr('L', a) => loop2(t, location, 0.until(a / 90).foldLeft(waypoint){ case (f, _) => f.rotate(AntiClockwise) })
      case Instr('R', a) => loop2(t, location, 0.until(a / 90).foldLeft(waypoint){ case (f, _) => f.rotate(Clockwise) })
      case _ => loop2(t, location, waypoint)
    }
  }

  part2(loop2(instrs, Point2D(0, 0), Point2D(10, 1)).manHattanDist(Point2D(0, 0)))
}
