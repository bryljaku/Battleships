package BattleShips

import scala.annotation.tailrec

case class Ship(positions: Set[Cell]) {

  def hit(cell: Cell): Ship = {
    val newPositions = positions.map(cellShip =>
      if (cellShip.x == cell.x && cellShip.y == cell.y) cellShip.copy(state = State.Hit)
      else cellShip)

    if (newPositions.dropWhile(x => x.state == State.Hit).isEmpty)
      this.copy(positions = newPositions.map(x => x.copy(state = State.Sink)))
    else
      this.copy(positions = newPositions)
  }
  def isSunk: Boolean = {
    positions.foreach(x => if (x.state == State.Miss) return false)
    true
  }

  def isOverlapping(ship: Ship): Boolean = {
    ship.positions.dropWhile(cell =>
      this.positions.dropWhile(tCell =>
      (tCell.x != cell.x) || (tCell.y != cell.y)).isEmpty).nonEmpty
  }

  def isTouched(cell: Cell): Boolean =
    positions.dropWhile(pos => pos.x != cell.x && pos.y != cell.y).nonEmpty

  def getCoordinates: Set[(Int, Int)] = {
    positions.map(x => (x.x, x.y))
  }
}

object Ship {
  def apply(positions: Set[Cell]) = new Ship(positions)

  def apply(x: Int, y: Int, direction: Char, length: Int):Ship = {
    def createPositions(x: Int, y: Int, direction: Char, len: Int): Set[Cell] = {
      @tailrec
      def helper(x: Int, y: Int, dir: Char, l: Int, pos: Set[Cell]): Set[Cell] = {
        if (l == 0) pos
        else {
          if (direction == 'H')
            helper(x + 1, y, dir, l - 1, pos + Cell(x, y, State.Occupied))
          else
            helper(x, y + 1, dir, l - 1, pos + Cell(x, y, State.Occupied))
        }
      }
      helper(x, y, direction, len, Set())
    }
    new Ship(createPositions(x, y, direction, length))
  }
}
