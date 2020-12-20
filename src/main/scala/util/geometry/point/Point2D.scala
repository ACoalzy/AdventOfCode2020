package util.geometry.point

import util.geometry.direction.{AntiClockwise, Clockwise, Direction2D, Rotation}

case class Point2D(x: Int, y: Int) {
  private def mutate(op: (Int, Int) => Int)(p: Point2D): Point2D = Point2D(op(x, p.x), op(y, p.y))

  def +(p: Point2D): Point2D = mutate(_ + _)(p)

  def -(p: Point2D): Point2D = mutate(_ - _)(p)

  def *(p: Point2D): Point2D = mutate(_ * _)(p)

  def *(i: Int): Point2D = mutate(_ * _)(Point2D(i, i))

  def rotate(rot: Rotation) = rot match {
    case Clockwise => Point2D(y, -x)
    case AntiClockwise => Point2D(-y, x)
  }

  def manHattanDist(p: Point2D): Int = math.abs(x - p.x) + math.abs(y - p.y)

  def addZ(z: Int) = Point3D(x, y, z)

  def neighbours: Set[Point2D] = Direction2D.all.map(_.mutation + this)

  def surroundingDirs = (for {
    x <- -1 to 1
    y <- -1 to 1
    if x != 0 || y != 0
  } yield Point2D(x, y)).toSet

  def surrounding: Set[Point2D] = surroundingDirs.map(_ + this)
}

object Point2D {
  def toCharMap(strings: Seq[String]): Map[Point2D, Char] = strings.map(_.zipWithIndex).zipWithIndex.flatMap {
    case (chars, y) => chars.map {
      case (c, x) => Point2D(x, strings.length - (y+1)) -> c
    }
  }.toMap
}
