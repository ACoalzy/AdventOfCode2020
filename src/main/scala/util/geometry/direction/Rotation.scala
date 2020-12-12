package util.geometry.direction

sealed trait Rotation

case object Clockwise extends Rotation
case object AntiClockwise extends Rotation
