package BattleShips

import scala.annotation.tailrec
import scala.util.Random

case class AI(name: String = "Scalony Pirat", fleet: Fleet = Fleet(), shotsGiven: Set[Cell] = Set(),
         shotsReceived: Set[Cell] = Set(), sinkShips: Set[Ship] = Set()) extends Player {
  val rand1 = Random
  val rand2 = Random

  override def shoot(): Cell = {
    @tailrec
    def randomShoot():Cell = {
      val x = rand1.nextInt(Board.SIZE - 1)
      val y = rand1.nextInt(Board.SIZE - 1)
      if (shotsGiven.filter(cell => cell.x == x && cell.y == y).isEmpty) Cell(x, y)
      else randomShoot()
    }
    def shootAroundIsolatedCell(cell: Cell): Cell = {
      val neighbours = getNeighbours(cell).filter(checkGuessedAndInBorder(_))
      if (neighbours.isEmpty) randomShoot()
      else neighbours.head
    }
    def shootInRow(cells: Set[Cell]): Cell = {
      def getPossibleCells: Set[Cell] = {
        val sortedCells = cells.toList.sortBy(_.x).sortBy(_.y)
        val t = sortedCells.last
        val h = sortedCells.head
        if (h.x != t.x)
          Set(Cell(h.x - 1, h.y), Cell(t.x + 1, t.y))
        else if (h.y != t.y)
          Set(Cell(h.x, h.y - 1), Cell(t.x, t.y + 1))
        else Set()
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
      Set(Cell(x,y + 1, State.Miss), Cell(x, y - 1, State.Miss), Cell(x + 1, y, State.Miss), Cell(x - 1, y, State.Miss))
      }
    def checkGuessedAndInBorder(cell: Cell): Boolean = {
        def guessed: Boolean = shotsGiven.filter(c => c.x == cell.x && c.y == cell.y).nonEmpty
        def inBorder: Boolean = cell.x >= 0 && cell.y >= 0 && cell.x < Board.SIZE && cell.y < Board.SIZE
      !guessed  && inBorder
      }


    val lastAccurateGuesses = shotsGiven.filter(_.state == State.Hit)
    if (lastAccurateGuesses.isEmpty) randomShoot()
    else if (lastAccurateGuesses.size == 1)
      shootAroundIsolatedCell(lastAccurateGuesses.head)
    else shootInRow(lastAccurateGuesses)
  }

  override def placeShips(shipsList: List[Int]): Player = {
    def placeShipsHelper(shipsList: List[Int], fleet: Fleet): Fleet = {
      if (shipsList.isEmpty) fleet
      else {
        val newFleet = fleet.addShip(createShip(shipsList.head))
        if (newFleet.isDefined) placeShipsHelper(shipsList.tail, newFleet.get)
        else placeShipsHelper(shipsList, fleet)
      }
    }
    this.copy(fleet = placeShipsHelper(shipsList, fleet))
  }
  def createShip(len: Int): Ship = {
    val x = Random.nextInt(Board.SIZE - 1)
    val y = Random.nextInt(Board.SIZE - 1)
    val orientation = Random.nextInt(2)
    orientation match {
      case 0 => Ship(x, y, 'H', len)
      case 1 => Ship(x, y, 'V', len)
    }
  }




  override def receiveShot(cell: Cell): (Player, Boolean, Option[Ship]) = {
    val (newFleet: Fleet, touched: Boolean, ship: Option[Ship]) = fleet.hit(cell)
    if (this.shotsReceived.exists(c => c.x == cell.x && c.y == cell.y)) {
      (this, touched, ship)
    } else {
      if (touched && ship.isDefined) {
        val newShotsReceived: Set[Cell] =
          (shotsReceived + cell.copy(state = State.Sink)).map(square => {
          val squareShip: Option[Cell] = ship.get.positions.find(squareShip =>
            squareShip.x == square.x && squareShip.y == square.y)
          squareShip.getOrElse(square)
        })
        (this.copy(fleet = newFleet, shotsReceived =
          newShotsReceived), touched, ship)
      } else if (touched && !ship.isDefined) {
        (this.copy(fleet = newFleet, shotsReceived =
          shotsReceived + cell.copy(state = State.Hit)), touched, ship)
      } else {
        (this.copy(fleet = newFleet, shotsReceived =
          shotsReceived + cell.copy(state = State.Miss)), touched, ship)
      }
    }
  }
  override def afterShooting(cell: Cell, hit: Boolean, sunkShip: Option[Ship]): Player = {
    if (shotsGiven.exists(s => s.x == cell.x && s.y == cell.y)) this
    else {
      if (hit) {
        if (sunkShip.isDefined) {
          val newSinkShips: Set[Ship] = sinkShips + sunkShip.get
          val newShotsGiven: Set[Cell] = shotsGiven + cell
          this.copy(shotsGiven = newShotsGiven.map(cell =>
            if (sunkShip.get.isTouched(cell)) cell.copy(state = State.Sink)
            else cell), sinkShips = newSinkShips)
        } else {
          this.copy(shotsGiven = shotsGiven + cell.copy(state = State.Hit))
        }
      }
      else {
        this.copy(shotsGiven = shotsGiven + cell.copy(state = State.Miss))
      }
    }
  }
}
