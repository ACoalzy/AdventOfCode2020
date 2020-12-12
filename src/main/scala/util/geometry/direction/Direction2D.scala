package util.geometry.direction

import util.geometry.point.Point2D

sealed trait Direction2D {
  def mutation: Point2D
}

object Direction2D {
  def fromChar(c: Char): Direction2D = c match {
    case 'U' => Up
    case 'D' => Down
    case 'L' => Left
    case 'R' => Right
  }

  def all: Set[Direction2D] = Set(Left, Right, Up, Down)
}

case object Left extends Direction2D {
  val mutation = Point2D(-1, 0)
}

case object Right extends Direction2D {
  val mutation = Point2D(1, 0)
}

case object Up extends Direction2D {
  val mutation = Point2D(0, 1)
}

case object Down extends Direction2D {
  val mutation = Point2D(0, -1)
}
