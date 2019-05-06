package main.scala.battleships

case class Direction(direction: Direction.Value){
}
object Direction extends Enumeration {
  val Horizontal, Vertical = Value
}