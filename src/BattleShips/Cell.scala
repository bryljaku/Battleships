package BattleShips

case class Cell(x: Int, y: Int, state: State.Value){
}
object State extends Enumeration {
  val Water, Hit, Miss, Occupied, Sink = Value
}