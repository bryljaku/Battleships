package BattleShips

import scala.annotation.tailrec
object Board extends App {
  val size = 5
  val shipsToPlace = (1,2,3,4)

  //  input
  def getCoordinates: (Int, Int) = {
    print("Enter x coordinate: ")
    val x = scala.io.StdIn.readInt()
    if (x <= 0 || x >= this.size) {
      println("wrong coordinate! Try again")
      getCoordinates
    }
    else {
      print("Enter y coordinate ")
      val y = scala.io.StdIn.readInt()
      if (y <= 0 || y >= this.size) {
        println("wrong coordinate! Try again")
        getCoordinates
      } else (x, y)
    }

  }

  @tailrec
  def getShipCoordinates(len: Int): (Int, Int, Char) = {
    @tailrec
    def getShipOrientation: Char = {
      println("Choose orientation of your ship: H - Horizontal, V - Vertical")
      val direction = scala.io.StdIn.readChar()
      if ("HV".contains(direction)) direction
      else {
        println("Wrong direction. Try again!")
        getShipOrientation
      }
    }

    def checkShipInBoard(x: Int, y: Int, dir: Char): Boolean = {
      def h(maxX: Int, maxY: Int): Boolean = maxX - x > 0 && maxY - y > 0
      if (dir == 'H')
        h(this.size - len, this.size)
      else
        h(this.size, this.size - len)
    }

    println(s"Choose starting point for your ship(length: $len)");
    val coordinates = getCoordinates
    val direction = getShipOrientation
    if (checkShipInBoard(coordinates._1, coordinates._2, direction))
      (coordinates._1, coordinates._2, direction)
    else {
      println("Your ship is out of boundaries, try again")
      getShipCoordinates(len)
    }
  }

  //  output
  def printTopRow: Unit = {
    print("|_ _|")
    for (x <- 0 to size - 2) print(s"|_${x}_|")
    println(s"|_${size - 1}_|")
  }

  def printLegend: Unit = {
    print("")
  }

  def showMyBoard(player: Player): Unit = {
    def printer(x: Int, y: Int): Unit = {
      if (x == 0) print(s"|_${y}_|")
      if (x >= size) {
        println()
        printer(0, y + 1)
      }
      else if (y >= size) println()
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
      }
      printer(x + 1, y)
    }
    printTopRow
    printer(0, 0)
  }

  def showEnemyBoard(player: Player): Unit = {
    @tailrec
    def printer(x: Int, y: Int): Unit = {
      if (x == 0) print(s"|_${y}_|")
      if (x >= size) {
        println()
        printer(0, y + 1)
      }
      else if (y >= size) println()
      else {
        val cell: Option[Cell] = player.shotsGiven.find(cell => cell.x == x && cell.y == y)
        if (cell.isDefined){
          cell.get.state match {
            case State.Miss => print("|_O_|")
            case State.Sink => print("|_S_|")
            case State.Hit => print("|_X_|")
            case _ => print("|_ _|")
          }
        }
        printer(x + 1, y)
      }
    }
    printTopRow
    printer(0, 0)
  }


}