package main.scala.battleships

import scala.annotation.tailrec
import Direction._

case class Ship(positions: Set[Cell]) {

  def hit(cell: Cell): Ship = {
    val newPositions = positions.map(cellShip =>
      if (cellShip.x == cell.x && cellShip.y == cell.y) cellShip.copy(state = State.Hit)
      else cellShip)

    if (newPositions.dropWhile(x => x.state == State.Hit).isEmpty)
      this.copy(positions = newPositions.map(x => x.copy(state = State.Sink)))
    else this.copy(positions = newPositions)
  }
  def isSunk: Boolean = {
    positions.foreach(x => if (x.state == State.Occupied) return false)
    true
  }

  def isOverlapping(ship: Ship): Boolean = {
    positions.dropWhile(cell =>
      !ship.isTouched(cell)).nonEmpty
  }

  def isTouched(cell: Cell): Boolean =
    positions.dropWhile(pos => pos.x != cell.x || pos.y != cell.y).nonEmpty

  def getCoordinates: Set[(Int, Int)] = {
    positions.map(x => (x.x, x.y))
  }
}

object Ship {
  def apply(positions: Set[Cell]) = new Ship(positions)

  def apply(tuple: Tuple4[Int, Int, Any, Int]): Ship =
    Ship(tuple._1, tuple._2, tuple._3, tuple._4)

  def apply(x: Int, y: Int, direction: Any, length: Int):Ship = {
      @tailrec
      def createPositions(x: Int, y: Int, len: Int, pos: Set[Cell]): Set[Cell] = {
        len match {
          case 0 => pos
          case _ =>
            direction match {
              case Horizontal => createPositions(x + 1, y, len - 1, pos + Cell(x, y, State.Occupied))
              case _ => createPositions(x, y + 1, len - 1, pos + Cell(x, y, State.Occupied))
            }
        }
    }
    new Ship(createPositions(x, y, length, Set()))
  }
}
