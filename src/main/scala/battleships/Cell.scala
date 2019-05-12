package main.scala.battleships

case class Cell(x: Int, y: Int, state: State.Value)

object Cell {
  def apply(x: Int, y: Int): Cell = 
    Cell(x, y, State.Water)
}
object State extends Enumeration {
  val Water, Hit, Miss, Occupied, Sink = Value
}
