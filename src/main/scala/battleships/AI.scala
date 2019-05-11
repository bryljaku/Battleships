package main.scala.battleships

import scala.annotation.tailrec
import scala.util.Random
import Board._
import State._

case class AI(name: String = "Blackbeard", fleet: Fleet = Fleet(), shotsGiven: Set[Cell] = Set(),
         shotsReceived: Set[Cell] = Set(), sinkShips: Set[Ship] = Set()) extends Player {
  val rand1 = Random

  override def shoot(): Cell = {
    @tailrec
    def randomShoot(): Cell = {
      val x = rand1.nextInt(SIZE - 1)
      val y = rand1.nextInt(SIZE - 1)
      shotsGiven.find(cell => cell.x == x && cell.y == y) match {
        case None => Cell(x, y)
        case _ => randomShoot()
      }
    }
    def shootAroundIsolatedCell(cell: Cell): Cell = {
      val neighbours = getNeighbours(cell).filter(checkGuessedAndInBorder).toList
      neighbours match {
        case Nil => randomShoot()
        case _ => neighbours.head
      }
    }
    def shootInRow(lastCorrectGuesses: Set[Cell]): Cell = {
      def getPossibleCells: Set[Cell] = {
        val sortedCells = lastCorrectGuesses.toList.sortBy(_.x).sortBy(_.y)
        println(sortedCells)
        val t = sortedCells.last
        val h = sortedCells.head
        (h.x, h.y) match {
          case (t.x, _) => Set(Cell(h.x - 1, h.y), Cell(t.x + 1, t.y))
          case (_, t.y) => Set(Cell(h.x, h.y - 1), Cell(t.x, t.y + 1))
          case _ => Set()
        }
      }
      def getOneCellFromPossibleCells(cells: Set[Cell]): Cell = {
        if (cells.isEmpty) randomShoot()
        else if (!checkGuessedAndInBorder(cells.head))
          getOneCellFromPossibleCells(cells.tail)
        else cells.head
      }
      getOneCellFromPossibleCells(getPossibleCells)
    }

    def getNeighbours(cell: Cell): Set[Cell] = {
      val x = cell.x
      val y = cell.y
      Set(Cell(x,y + 1), Cell(x, y - 1), Cell(x + 1, y), Cell(x - 1, y))
      }
    def checkGuessedAndInBorder(cell: Cell): Boolean = {
      def guessed: Boolean = shotsGiven.exists(c => c.x == cell.x && c.y == cell.y)
      def inBorder: Boolean = cell.x >= 0 && cell.y >= 0 && cell.x < SIZE && cell.y < SIZE
      !guessed  && inBorder
      }

    val lastAccurateGuesses = shotsGiven.filter(_.state == Hit)
    lastAccurateGuesses.size match {
      case 0 => randomShoot()
      case 1 => shootAroundIsolatedCell(lastAccurateGuesses.head)
      case _ => shootInRow(lastAccurateGuesses)
    }
  }

  override def placeShips(shipsList: List[Int]): Player = {
    def placeShipsHelper(shipsList: List[Int], fleet: Fleet): Fleet = {
      shipsList match {
        case Nil => fleet
        case _ => {
          val newFleet = fleet.addRandomShip(shipsList.head)
          newFleet match {
            case None => placeShipsHelper(shipsList, fleet)
            case _ => placeShipsHelper(shipsList.tail, newFleet.get)
          }
        }
      }
    }

    copy(fleet = placeShipsHelper(shipsList, fleet))
  }


  override def receiveShot(cell: Cell): (Player, Boolean, Option[Ship]) = {
    val (newFleet: Fleet, touched: Boolean, ship: Option[Ship]) = fleet.hit(cell)
    if (shotsReceived.exists(s => s.x == cell.x && s.y == cell.y)) {
      (this, touched, ship)
    } else { 
      ship match {
        case Some(s) =>
          val newShotsReceived: Set[Cell] = (shotsReceived + cell.copy(state = Sink)).map(square => {
          val squareShip: Option[Cell] = s.positions.find(squareShip => squareShip.x == square.x && squareShip.y == square.y)
          squareShip.getOrElse(square)
          })
          (copy(fleet = newFleet, shotsReceived = newShotsReceived), touched, ship)
        case _ if touched => 
          (copy(fleet = newFleet, shotsReceived = shotsReceived + cell.copy(state = Hit)), touched, ship)
        case _  => 
          (copy(fleet = newFleet, shotsReceived = shotsReceived + cell.copy(state = Miss)), touched, ship)
      }
    }
  }
  override def afterShooting(cell: Cell, hit: Boolean, sunkShip: Option[Ship]): Player = {
    if (shotsGiven.exists(s => s.x == cell.x && s.y == cell.y)) this
    else {
      sunkShip match {
        case Some(s) => 
          val newSinkShips: Set[Ship] = sinkShips + s
          val newShotsGiven: Set[Cell] = shotsGiven + cell
          copy(shotsGiven = newShotsGiven.map(cell =>
            if (s.isTouched(cell)) cell.copy(state = Sink)
            else cell), sinkShips = newSinkShips)
        case _ if hit => copy(shotsGiven = shotsGiven + cell.copy(state = Hit))
        case _ => copy(shotsGiven = shotsGiven + cell.copy(state = Miss))
      }
    }
  }
}
