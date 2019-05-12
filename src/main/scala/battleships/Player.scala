package main.scala.battleships

trait Player {
  val name: String
  val fleet: Fleet
  val shotsGiven: Set[Cell]
  val shotsReceived: Set[Cell]
  val sinkShips: Set[Ship]

  def shoot(): Cell
  def placeShips(shipsList: List[Int]): Player
  def afterShooting(cell: Cell, hit: Boolean, sunkShip: Option[Ship] = None): Player
  def receiveShot(cell: Cell): ReceiveShotValue
  def checkLose(): Boolean = fleet.ships.size == sinkShips.size

}

  case class ReceiveShotValue(player: Player, isHit: Boolean, sunkShip: Option[Ship])
