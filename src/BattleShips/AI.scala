package BattleShips

import scala.annotation.tailrec
import scala.util.Random

case class AI(name: String = "Scalony Pirat", fleet: Fleet = Fleet(), shotsGiven: Set[Cell] = Set(),
         shotsReceived: Set[Cell] = Set(), sinkShips: Set[Ship] = Set()) extends Player {


  override def shoot(): Cell = {
    @tailrec
    def randomShoot():Cell = {
      val rand1 = Random
      val rand2 = Random
      val x = rand1.nextInt(Board.SIZE - 1)
      val y = rand2.nextInt(Board.SIZE - 1)
      if (shotsGiven.dropWhile(cell => cell.x != x && cell.y != y).isEmpty) Cell(x, y)
      else randomShoot()
    }
    def shootAroundIsolatedCell(cell: Cell): Cell = {
      val neighbours = getNeighbours(cell).filter(!checkGuessedAndInBorder(_))
      if (neighbours.isEmpty) randomShoot()
      else neighbours.head
    }
    def shootInRow(cells: Set[Cell]): Cell = {
      def getPossibleCells: Set[Cell] = {
        val sortedCells = cells.toList.sortWith(_.x < _.x).sortWith(_.y < _.y)
        val t = sortedCells.last
        val h = sortedCells.head
        if (sortedCells(0).x != sortedCells(1).x)
          Set(Cell(h.x - 1, h.y), Cell(h.x + 1, h.y))
        else if (sortedCells(0).y != sortedCells(1).y)
          Set(Cell(h.x, h.y - 1), Cell(h.x, h.y + 1))
        else Set()
      }
      def getOneCellFromPossibleCells(cells: Set[Cell]): Cell = {
        if (cells.isEmpty) randomShoot()
        else if (checkGuessedAndInBorder(cells.head))
          getOneCellFromPossibleCells(cells.tail)
        else cells.head
      }
      getOneCellFromPossibleCells(getPossibleCells)
    }

    def getNeighbours(cell: Cell): Set[Cell] = {
      val x = cell.x
      val y = cell.y
      Set(Cell(x,y + 1), Cell(x, y - 1), Cell(x + 1, y), Cell(x - 1, y)).filter(c =>
        c.x >= 0 && c.x < Board.SIZE && c.y >= 0 && c.y < Board.SIZE)
      }
    def checkGuessedAndInBorder(cell: Cell): Boolean = {
        shotsGiven.dropWhile(c => c.x != cell.x && c.y != cell.y).nonEmpty &&
          cell.x >= 0 && cell.y >= 0 && cell.x < Board.SIZE && cell.y < Board.SIZE
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
        Board.showMyBoard(this.copy(fleet = fleet))
        val shipCoordinates = Board.getShipCoordinates(shipsList.head)
        val newFleet = fleet.addShip(Ship(shipCoordinates))
        if (newFleet.isDefined) placeShipsHelper(shipsList.tail, newFleet.get)
        else {
          println("Ship is overlapping another ship from your fleet! Try again")
          placeShipsHelper(shipsList, fleet)
        }
      }
    }
    println("Placement of ships!")
    this.copy(fleet = placeShipsHelper(shipsList, fleet))
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
          println(s"Ship with a size of ${sunkShip.get.positions.size} destroyed!")
          val newSinkShips: Set[Ship] = sinkShips + sunkShip.get
          val newShotsGiven: Set[Cell] = shotsGiven + cell
          this.copy(shotsGiven = newShotsGiven.map(cell =>
            if (sunkShip.get.isTouched(cell)) cell.copy(state = State.Sink)
            else cell), sinkShips = newSinkShips)
        } else {
          println("Hit!")
          this.copy(shotsGiven = shotsGiven + cell.copy(state = State.Hit))
        }
      }
      else {
        println("Miss")
        this.copy(shotsGiven = shotsGiven + cell.copy(state = State.Miss))
      }
    }
  }
}
