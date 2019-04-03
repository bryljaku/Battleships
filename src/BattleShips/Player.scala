package BattleShips

trait Player {
  val name: String
  val fleet: Fleet
  val shotsGiven: Set[Cell]
  val shotsReceived: Set[Cell]
  val sinkShips: Set[Ship]

  def placeShip(ship: Ship): Player
  def guess(cell: Cell): Cell
  def receive(cell: Cell)
  def didLose(): Boolean = fleet.ships.size == sinkShips.size

}
