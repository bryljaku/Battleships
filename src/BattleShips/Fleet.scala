package BattleShips

case class Fleet(ships: Set[Ship]) {
  def addShip(s: Ship): Fleet = {
    if (!isOverlapping(s))
      this
    else
      this.copy(ships = ships + s)
  }
  def hit(cell: Cell): Fleet = {
    if (!isTouched(cell))
      return this
    this.copy(ships = ships.map(x =>
      if (x.isTouched(cell)) x.hit(cell)
      else x))
  }
  def isTouched(cell: Cell): Boolean = {
    ships.dropWhile(x => x.isTouched(cell)).nonEmpty
  }
  def isOverlapping(s: Ship): Boolean = {
    ships.dropWhile(ship => !ship.isOverlapping(s)).nonEmpty
  }
  def getShipsCoordinates: Set[(Int, Int)] = {
    ships.flatMap(ship => ship.getCoordinates)
  }
  def getShipsCells: Set[Cell] = {
    ships.flatMap(ship => ship.positions)
  }
}
