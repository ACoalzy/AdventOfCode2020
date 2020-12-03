package util.geometry.point

import util.geometry.direction.Direction2D

case class Point2D(x: Int, y: Int) {
  private def mutate(op: (Int, Int) => Int)(p: Point2D): Point2D = Point2D(op(x, p.x), op(y, p.y))

  def +(p: Point2D): Point2D = mutate(_ + _)(p)

  def -(p: Point2D): Point2D = mutate(_ - _)(p)

  def *(p: Point2D): Point2D = mutate(_ * _)(p)

  def *(i: Int): Point2D = mutate(_ * _)(Point2D(i, i))

  def rotate(dir: Long) = if (dir == 1L) Point2D(y, -x) else Point2D(-y, x)

  def manHattanDist(p: Point2D): Int = math.abs(x - p.x) + math.abs(y - p.y)

  def addZ(z: Int) = Point3D(x, y, z)

  def neighbours: Set[Point2D] = Direction2D.all.map(_.mutation + this)
}
