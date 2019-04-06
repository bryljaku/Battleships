package BattleShips

trait Player {
  val name: String
  val fleet: Fleet
  val shotsGiven: Set[Cell]
  val shotsReceived: Set[Cell]
  val sinkShips: Set[Ship]

  def placeShips(lengths: List[Int]): Player
  def guess(cell: Cell): Cell
  def receiveShot(cell: Cell)
  def checkLose(): Boolean = fleet.ships.size == sinkShips.size

}
