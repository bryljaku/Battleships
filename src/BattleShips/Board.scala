package BattleShips

import scala.annotation.tailrec
object Board {
  final val SIZE = 6
  val shipsToPlace = List(1, 2, 3, 4)

  //  input
  def getCoordinates: (Int, Int) = {
    print("Enter x coordinate: ")
    val x = scala.io.StdIn.readLine()(0).asDigit
    if (x < 0 || x >= this.SIZE) {
      println("wrong coordinate! Try again")
      getCoordinates
    }
    else {
      print("Enter y coordinate ")
      val y = scala.io.StdIn.readLine()(0).asDigit
      if (y < 0 || y >= this.SIZE) {
        println("wrong coordinate! Try again")
        getCoordinates
      } else {
        println(s"($x, $y)")
        (x, y)
      }
    }
  }

  @tailrec
  def getShipCoordinates(len: Int): (Int, Int, Char, Int) = {
    @tailrec
    def getShipOrientation: Char = {
      println("Choose orientation of your ship: H - Horizontal, V - Vertical")
      val direction = scala.io.StdIn.readChar()
      if ("HV".contains(direction.toUpper))
        direction
      else {
        println("Wrong direction. Try again!")
        getShipOrientation
      }
    }
    def checkShipInBoard(x: Int, y: Int, dir: Char): Boolean = {
      def h(maxX: Int, maxY: Int): Boolean = maxX - x >= 0 && maxY - y >= 0
      if (dir == 'H')
        h(this.SIZE - len, this.SIZE)
      else
        h(this.SIZE, this.SIZE - len)
    }
    println(s"Choose starting point for your ship(length: $len)");
    val coordinates = getCoordinates
    val direction = getShipOrientation
    if (checkShipInBoard(coordinates._1, coordinates._2, direction))
      (coordinates._1, coordinates._2, direction, len)
    else {
      println("Your ship is out of boundaries, try again")
      getShipCoordinates(len)
    }
  }


  //  output
  def printTopRow: Unit = {
    print("|_ _|")
    for (x <- 0 to SIZE - 2) print(s"|_${x}_|")
    println(s"|_${SIZE - 1}_|")
  }

  def printLegend: Unit = {
    print("X - Destroyed, + - Hit, O - Miss, $ - Ship")
  }

  def showMyBoard(player: Player): Unit = {
    def printer(x: Int, y: Int): Unit = {
      if (x == 0 && y < SIZE) print(s"|_${y}_|")
      if (x >= SIZE) {
        println()
        printer(0, y + 1)
      } else if (y >= SIZE)
        println()
      else {
        val cell: Option[Cell] = player.fleet.getShipsCells.find(c => c.x == x && c.y == y)
        if (cell.isDefined) {
          cell.get.state match {
            case State.Miss => print("|_O_|")
            case State.Sink => print("|_X_|")
            case State.Hit => print("|_+_|")
            case State.Occupied => print("|_$_|")
            case _ => print("|_ _|")}
        } else print("|_ _|")
        printer(x + 1, y)
      }
    }
    println("My board")
    printTopRow
    printer(0, 0)
  }

  def showEnemyBoard(player: Player): Unit = {
    @tailrec
    def printer(x: Int, y: Int): Unit = {
      if (x == 0 && y < SIZE) print(s"|_${y}_|")
      if (x >= SIZE) {
        println()
        printer(0, y + 1)
      } else if (y >= SIZE)
        println()
      else {
        val cell: Option[Cell] = player.shotsGiven.find(cell => cell.x == x && cell.y == y)
        if (cell.isDefined){
          cell.get.state match {
            case State.Miss => print("|_O_|")
            case State.Sink => print("|_X_|")
            case State.Hit => print("|_+_|")
            case _ => print("|_ _|")
          }
        } else print("|_ _|")
        printer(x + 1, y)
      }
    }
    if (player.isInstanceOf[Human])
      println("Guesses")
    else if (player.isInstanceOf[AI])
      println("AI guesses")
    printTopRow
    printer(0, 0)
  }
}