package BattleShips

case class Human(name: String = "Kapitan Pazur", fleet: Fleet = Fleet(), shotsGiven: Set[Cell] = Set(),
            shotsReceived: Set[Cell] = Set(), sinkShips: Set[Ship] = Set()) extends Player {

  //  this.shoot -> Board -> otherPlayer.receiveShot -> Board -> this.afterShooting
  override def shoot(): Cell = {
    println("Your turn to shoot!")
    val (x, y) = Board.getCoordinates
    Cell(x, y)
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


  override def afterShooting(cell: Cell, hit: Boolean, sunkShip: Option[Ship]): Player = {
    if (shotsGiven.exists(s => s.x == cell.x && s.y == cell.y)) this
    else {
      if (hit) {
        if (sunkShip.isDefined) {
          println(s"Ship with a size of ${sunkShip.get.positions.size} destroyed!")
          val newSinkShips: Set[Ship] = sinkShips + sunkShip.get
          val newShotsGiven: Set[Cell] = shotsGiven + cell
          this.copy(shotsGiven = newShotsGiven.map(cell => if (sunkShip.get.isTouched(cell)) cell.copy(state = State.Sink) else cell), sinkShips = newSinkShips)
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

  override def receiveShot(cell: Cell): (Player, Boolean, Option[Ship]) = {
    val (newFleet: Fleet, touched: Boolean, ship: Option[Ship]) = fleet.hit(cell)
    if (this.shotsReceived.exists(s => s.x == cell.x && s.y == cell.y)) {
      (this, touched, ship)
    } else {
      if (touched && ship.isDefined) {
        val newShotsReceived: Set[Cell] = (shotsReceived + cell.copy(state = State.Sink)).map(square => {
          val squareShip: Option[Cell] = ship.get.positions.find(squareShip => squareShip.x == square.x && squareShip.y == square.y)
          squareShip.getOrElse(square)
        })
        (this.copy(fleet = newFleet, shotsReceived = newShotsReceived), touched, ship)
      } else if (touched && !ship.isDefined) {
        (this.copy(fleet = newFleet, shotsReceived = shotsReceived + cell.copy(state = State.Hit)), touched, ship)
      } else {
        (this.copy(fleet = newFleet, shotsReceived = shotsReceived + cell.copy(state = State.Miss)), touched, ship)
      }
    }
  }
}
