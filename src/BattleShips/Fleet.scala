package BattleShips

case class Fleet(ships: Set[Ship] = Set()) {

  def addShip(ship: Ship): Option[Fleet] = {
    if(!isOverlapping(ship)) {
      Some(this.copy(ships = this.ships + ship))
    } else None

  }
  def hit(cell: Cell): (Fleet, Boolean, Option[Ship]) = {
       val newShips = ships.map(ship => if (ship.isTouched(cell)) ship.hit(cell) else ship)
       val touched = newShips.dropWhile(ship => !ship.isTouched(cell))
       val ship: Option[Ship] = if (touched.nonEmpty) {
         if (touched.head.isSunk) Some(touched.head)
         else None
       } else None

    (this.copy(ships = newShips), touched.nonEmpty, ship)
  }
  def isHit(cell: Cell): Boolean = {
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
