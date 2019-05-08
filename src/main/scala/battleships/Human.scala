package main.scala.battleships

import main.scala.battleships.Board.{SIZE}
import Direction._
import State._
import scala.annotation.tailrec
import scala.io.StdIn.{readChar, readLine}

case class Human(name: String = "Kapitan Pazur", fleet: Fleet = Fleet(), shotsGiven: Set[Cell] = Set(),
            shotsReceived: Set[Cell] = Set(), sinkShips: Set[Ship] = Set()) extends Player {

  //  shoot -> Board -> otherPlayer.receiveShot -> Board -> afterShooting
  override def shoot(): Cell = {
    println("Your turn to shoot!")
    val (x, y) = getCoordinates
    Cell(x, y)
  }
  override def placeShips(shipsList: List[Int]): Player = {
    def placeShipsHelper(shipsList: List[Int], fleet: Fleet): Fleet = {
      if (shipsList.isEmpty) fleet
      else {
        Board.showMyBoard(copy(fleet = fleet))
        val shipCoordinates = getShipCoordinates(shipsList.head)
        val newFleet = fleet.addShip(Ship(shipCoordinates))
        newFleet match {
          case Some(newF) => placeShipsHelper(shipsList.tail, newF)
          case _ =>
          println("Ship is overlapping another ship from your fleet! Try again")
          placeShipsHelper(shipsList, fleet)
            
        }
      }
    }
    println("Placement of ships!")
    copy(fleet = placeShipsHelper(shipsList, fleet))
  }


  override def afterShooting(cell: Cell, hit: Boolean, sunkShip: Option[Ship]): Player = {
    if (shotsGiven.exists(s => s.x == cell.x && s.y == cell.y)) this
    else {
      if (hit) {
        sunkShip match {
          case Some(s) => 
            println(s"Ship with a size of ${s.positions.size} destroyed!")
            val newSinkShips: Set[Ship] = sinkShips + s
            val newShotsGiven: Set[Cell] = shotsGiven + cell
            copy(shotsGiven = newShotsGiven.map(cell => if (s.isTouched(cell)) cell.copy(state = Sink) else cell), sinkShips = newSinkShips)
          case _ => 
            println("Hit!")
            copy(shotsGiven = shotsGiven + cell.copy(state = Hit)) 
        }
      }
      else {
        println("Miss")
        copy(shotsGiven = shotsGiven + cell.copy(state = Miss))
      }
    }
  }

  override def receiveShot(cell: Cell): (Player, Boolean, Option[Ship]) = {
    val (newFleet: Fleet, touched: Boolean, ship: Option[Ship]) = fleet.hit(cell)
    if (shotsReceived.exists(s => s.x == cell.x && s.y == cell.y)) {
      (this, touched, ship)
    } else { touched match {
      case true => ship match {
        case None => (copy(fleet = newFleet, shotsReceived = shotsReceived + cell.copy(state = Hit)), touched, ship)
        case _ =>
          val newShotsReceived: Set[Cell] = (shotsReceived + cell.copy(state = Sink)).map(square => {
          val squareShip: Option[Cell] = ship.get.positions.find(squareShip => squareShip.x == square.x && squareShip.y == square.y)
          squareShip.getOrElse(square)
        })
          (copy(fleet = newFleet, shotsReceived = newShotsReceived), touched, ship)
      }
      case false => (copy(fleet = newFleet, shotsReceived = shotsReceived + cell.copy(state = Miss)), touched, ship)
    }
    }
  }
  @tailrec
  final def getCoordinates: (Int, Int) = {
    print("Enter x coordinate: ")
    val x = readLine()(0).asDigit //io.StdIn.readLine()(0).asDigit
    if (x < 0 || x >= SIZE) {
      println("wrong coordinate! Try again")
      getCoordinates
    }
    else {
      print("Enter y coordinate ")
      val y = readLine()(0).asDigit
      if (y < 0 || y >= SIZE) {
        println("wrong coordinate! Try again")
        getCoordinates
      } else {
        println(s"($x, $y)")
        (x, y)
      }
    }
  }

  @tailrec
  final def getShipCoordinates(len: Int): (Int, Int, Any, Int) = {
    def getShipDirection: Any = {
      println("Choose orientation of your ship: H - Horizontal, V - Vertical")
      readChar.toUpper match {
        case 'H' => Horizontal
        case 'V' => Vertical
        case _ =>
          println("Wrong direction. Try again!")
          getShipDirection
      }
    }
    def checkShipInBoard(x: Int, y: Int, dir: Any): Boolean = {
      def helper(maxX: Int, maxY: Int): Boolean = maxX - x >= 0 && maxY - y >= 0
      dir match {
        case Horizontal => helper(SIZE - len, SIZE)
        case _ => helper(SIZE, SIZE - len)
      }
    }
    println(s"Choose starting point for your ship(length: $len)");
    val coordinates = getCoordinates
    val direction = len match {
      case 1 => Horizontal
      case _ => getShipDirection
    }
    if (checkShipInBoard(coordinates._1, coordinates._2, direction))
      (coordinates._1, coordinates._2, direction, len)
    else {
      println("Your ship is out of boundaries, try again")
      getShipCoordinates(len)
    }
  }

}
