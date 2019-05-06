package main.scala.battleships

case class Cell(x: Int, y: Int, state: State.Value = State.Water){
}
object State extends Enumeration {
  val Water, Hit, Miss, Occupied, Sink = Value
}