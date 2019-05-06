package main.scala.battleships
import _root_.scala.annotation.tailrec
//import com.typesafe.config.{ Config, ConfigFactory }

object Board {
//  val conf = ConfigFactory.load
  val SIZE = 6
  val shipsToPlace = List(1)//conf.getString("battleships.size")
  def printTopRow: Unit = {
    print("|_ _|")
    for (x <- 0 to SIZE - 2) print(s"|_${x}_|")
    println(s"|_${SIZE - 1}_|")
  }

  def printLegend: Unit = {
    print("X - Destroyed, + - Hit, O - Miss, $ - Ship")
  }

  def showMyBoard(player: Player): Unit = {
    val shipsCells = player.fleet.getShipsCells
    def printer = {
      @tailrec
      def printerHelper(x: Int, y: Int, remainedShipsCells: Set[Cell]): Unit = {
       if (x == 0 && y < SIZE) print(s"|_${y}_|")
        if (x >= SIZE) {
         println()
          printerHelper(0, y + 1, remainedShipsCells)
       } else if (y >= SIZE)
         println()
        else {
          val cell: Option[Cell] = remainedShipsCells.find(c => c.x == x && c.y == y)
         if (cell.isDefined) {
           cell.get.state match {
              case State.Miss => print("|_O_|")
              case State.Sink => print("|_X_|")
              case State.Hit => print("|_+_|")
              case State.Occupied => print("|_$_|")
              case _ => print("|_ _|")}
         } else print("|_ _|")
         printerHelper(x + 1, y, shipsCells.filter(c => c.x != x || c.y != y))
        }
      }
      printerHelper(0, 0, shipsCells)
    }
    println("My board")
    printTopRow
    printer
  }

  def showEnemyBoard(player: Player): Unit = {

    def printer = {
    @tailrec
    def printerHelper(x: Int, y: Int, shotsGivenRemained: Set[Cell]): Unit = {
      if (x == 0 && y < SIZE) print(s"|_${y}_|")
      if (x >= SIZE) {
        println()
        printerHelper(0, y + 1, shotsGivenRemained)
      } else if (y >= SIZE)
        println()
      else {
        val cell: Option[Cell] = shotsGivenRemained.find(cell => cell.x == x && cell.y == y)
        if (cell.isDefined){
          cell.get.state match {
            case State.Miss => print("|_O_|")
            case State.Sink => print("|_X_|")
            case State.Hit => print("|_+_|")
            case _ => print("|_ _|")
          }
        } else print("|_ _|")
        printerHelper(x + 1, y, shotsGivenRemained.filter(cell => cell.x != x || cell.y != y))
      }
    }
      printerHelper(0, 0, player.shotsGiven)
    }
    player match {
      case _: Human => println(s"${player.name} Guesses")
      case _: AI => println(s"${player.name}(AI) guesses")
    }
    printTopRow
    printer
  }
}
