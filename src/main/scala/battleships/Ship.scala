package main.scala.battleships

import scala.annotation.tailrec
import Direction._
import State._

case class Ship(positions: Set[Cell]) {
  def hit(cell: Cell): Ship = {
    val newPositions = positions.map(cellShip => cellShip match {
      case Cell(cell.x, cell.y, _) => cellShip.copy(state = Hit)
      case _ => cellShip
    })
    newPositions.find(x => x.state != Hit) match {
      case None => copy(positions = newPositions.map(x => x.copy(state = Sink)))
      case _ => copy(positions = newPositions)
    }
  }
  def isSunk: Boolean = {
    !positions.exists(x => x.state == Occupied)
  }

  def isOverlapping(ship: Ship): Boolean = {
    positions.exists(cell => ship.isTouched(cell))
  }

  def isTouched(cell: Cell): Boolean =
    positions.exists(pos => pos.x == cell.x && pos.y == cell.y)

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
              case Horizontal => createPositions(x + 1, y, len - 1, pos + Cell(x, y, Occupied))
              case _ => createPositions(x, y + 1, len - 1, pos + Cell(x, y, Occupied))
            }
        }
    }
    new Ship(createPositions(x, y, length, Set()))
  }
}
