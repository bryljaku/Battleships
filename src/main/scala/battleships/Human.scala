package main.scala.battleships

import main.scala.battleships.Board.{SIZE}
import Direction._
import State._
import scala.annotation.tailrec
import scala.io.StdIn.{readChar, readLine}

case class Human(name: String = "Sparrow", fleet: Fleet = Fleet(), shotsGiven: Set[Cell] = Set(),
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
        sunkShip match {
          case Some(s) => 
            println(s"Ship with a size of ${s.positions.size} destroyed!")
            val newSinkShips: Set[Ship] = sinkShips + s
            val newShotsGiven: Set[Cell] = shotsGiven + cell
            copy(shotsGiven = newShotsGiven.map(cell => if (s.isTouched(cell)) cell.copy(state = Sink) else cell), sinkShips = newSinkShips)
          case _ if hit => 
            println("Hit!")
            copy(shotsGiven = shotsGiven + cell.copy(state = Hit)) 
          case _ => println("Miss")
            copy(shotsGiven = shotsGiven + cell.copy(state = Miss))
        }
      }
  }

  override def receiveShot(cell: Cell): ReceiveShotValue = {
    val hitValue = fleet.hit(cell)
    if (shotsReceived.exists(s => s.x == cell.x && s.y == cell.y)) {
      ReceiveShotValue(this, hitValue.isHit, hitValue.sunkShip)
    } else { 
      hitValue.sunkShip match {
        case Some(s) =>
          val newShotsReceived: Set[Cell] = (shotsReceived + cell.copy(state = Sink)).map(square => {
          val squareShip: Option[Cell] = s.positions.find(squareShip => squareShip.x == square.x && squareShip.y == square.y)
          squareShip.getOrElse(square)
          })
          ReceiveShotValue(copy(fleet = hitValue.fleet, shotsReceived = newShotsReceived), hitValue.isHit, hitValue.sunkShip)
        case _ if hitValue.isHit => 
          ReceiveShotValue(copy(fleet = hitValue.fleet, shotsReceived = shotsReceived + cell.copy(state = Hit)), hitValue.isHit, hitValue.sunkShip)
        case _  => 
          ReceiveShotValue(copy(fleet = hitValue.fleet, shotsReceived = shotsReceived + cell.copy(state = Miss)), hitValue.isHit, hitValue.sunkShip)
        
      }
    }
  }
  
  def getCoordinates: (Int, Int) = {
    def helper: Int = {
      try {
      val x = readLine().toInt
      if (x < 0 || x >= SIZE){
        println("wrong coordinate, try again")
        helper
      } else x
      }
      catch {
        case _: Throwable => println("wrong input, try again") 
        helper
      }
    }
    println("Enter x: ")
    val x = helper
    println("Enter y: ")
    val y = helper
    println(s"($x,$y)")
    (x,y)
  }

   def getShipCoordinates(len: Int): ShipCoordinates = {
    @tailrec
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
    @tailrec
    def getShipCoordinatesHelper: ShipCoordinates = { 
    println(s"Choose starting point for your ship(length: $len)");
    val coordinates = getCoordinates
    val direction = len match {
      case 1 => Horizontal
      case _ => getShipDirection
    }
    if (checkShipInBoard(coordinates._1, coordinates._2, direction))
      ShipCoordinates(coordinates._1, coordinates._2, direction, len)
    else {
      println("Your ship is out of boundaries, try again")
      getShipCoordinatesHelper
      }
    }

    getShipCoordinatesHelper
  }
}
