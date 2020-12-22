package exercises

import util.DayN
import util.geometry.direction._
import util.geometry.point.Point2D

object Day20 extends DayN {
  override val num = 20

  case class Tile(id: Long, grid: Map[Point2D, Char]) {
    private val maxX = grid.keys.map(_.x).max
    private val maxY = grid.keys.map(_.y).max

    val bottom = grid.filter(_._1.y == 0).toList.sortBy(_._1.x).map(_._2)
    val top = grid.filter(_._1.y == maxY).toList.sortBy(_._1.x).map(_._2)
    val left = grid.filter(_._1.x == 0).toList.sortBy(_._1.y).map(_._2)
    val right = grid.filter(_._1.x == maxX).toList.sortBy(_._1.y).map(_._2)

    val edges = List(bottom, top, left, right)

    def flip: Tile = Tile(
      id = id,
      grid = grid.map { case (p, c) => Point2D(maxX - p.x, p.y) -> c }
    )
    def rotate(times: Int): Tile = {
      Tile(
        id = id,
        grid = grid.map { case (p, c) => (0 until times).foldLeft(p){ case (p, _) => p.rotate(Clockwise) + Point2D(0, maxY) } -> c }
      )
    }

    def edge(dir: Direction2D): List[Char] = dir match {
      case Up => top
      case Down => bottom
      case Right => right
      case Left => left
    }

    override def toString: String = {
      val xs = 0 to maxX
      val ys = 0 to maxY
      val gridString = ys.reverse.map(y => xs.map(x => grid(Point2D(x, y))).mkString).mkString("\n")
      s"Tile: $id\n" + gridString
    }
  }

  val idRegex = """.* (\d+).*""".r

  val tiles = lines.mkString("\n").split("\n\n").map(s => {
    val lines = s.split("\n")
    lines.head match {
      case idRegex(id) =>
        Tile(id.toLong, Point2D.toCharMap(lines.tail))
    }
  }).toSet

  val corners = tiles.filter(t => t.edges.count(e => {
    val edges = (tiles - t).flatMap(_.edges)
    (edges ++ edges.map(_.reverse)).contains(e)
  }) == 2)

  part1(corners.map(_.id).product)

  val rotations: List[Int] = List(0, 1, 2, 3)
  val flips: List[Boolean] = List(false, true)

  def findGrid(tiles: Set[Tile]): Option[Map[Point2D, Tile]] = {
    val width = math.sqrt(tiles.size).toInt
    val points = for {
      x <- 0 until width
      y <- 0 until width
    } yield Point2D(x, y)

    def loop(location: Point2D, permutations: Set[Tile], rem: Set[Tile], grid: Map[Point2D, Tile]): Option[Map[Point2D, Tile]] = {
      if (rem.isEmpty && permutations.nonEmpty) Some(grid.updated(location, permutations.head))
      else if (permutations.nonEmpty) ???
      else None
    }

//    loop(Point2D(0, 0), tiles, Map())
    ???
  }

}
