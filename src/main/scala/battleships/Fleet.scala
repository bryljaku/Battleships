package main.scala.battleships

import scala.util.Random
import Direction._

case class Fleet(ships: Set[Ship] = Set()) {

  def addShip(ship: Ship): Option[Fleet] = {
    if(!isOverlapping(ship)) {
      Some(copy(ships = ships + ship))
    } else None
  }

  def addRandomShip(len: Int): Option[Fleet] = {
    val x = Random.nextInt(Board.SIZE - 1)
    val y = Random.nextInt(Board.SIZE - 1)
    val orientation = Random.nextInt(2)
    orientation match {
      case 0 => addShip(Ship(x, y, Horizontal, len))
      case 1 => addShip(Ship(x, y, Vertical, len))
    }
  }
  def hit(cell: Cell): (Fleet, Boolean, Option[Ship]) = {
       val newShips = ships.map(ship => if (ship.isTouched(cell)) ship.hit(cell) else ship)
       val touched = newShips.find(ship => ship.isTouched(cell))
       val ship: Option[Ship] = if (touched.nonEmpty) {
         if (touched.get.isSunk) Some(touched.head)
         else None
       } else None

    (this.copy(ships = newShips), touched.nonEmpty, ship)
  }
  def isHit(cell: Cell): Boolean = {
    ships.exists(_.isTouched(cell))
  }
  def isOverlapping(s: Ship): Boolean = {
    ships.exists(_.isOverlapping(s))
  }
  def getShipsCoordinates: Set[(Int, Int)] = {
    ships.flatMap(_.getCoordinates)
  }
  def getShipsCells: Set[Cell] = {
    ships.flatMap(_.positions)
  }
}
