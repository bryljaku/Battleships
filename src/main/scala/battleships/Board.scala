package main.scala.battleships
import _root_.scala.annotation.tailrec
import State._
//import com.typesafe.config.{ Config, ConfigFactory }

object Board {
//  val conf = ConfigFactory.load
  val SIZE = 6
  val shipsToPlace = List(1, 3, 4)//conf.getString("battleships.size")
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

       (x, y) match {
         case (SIZE, _) =>
          println
          printerHelper(0, y + 1, remainedShipsCells)
         case (_, SIZE) =>
          println
        case _ => 
          val cell = remainedShipsCells.find(c => c.x == x && c.y == y)
          cell match {
            case Some(c) => c.state match {
              case Miss => print("|_O_|")
              case Sink => print("|_X_|")
              case Hit => print("|_+_|")
              case Occupied => print("|_$_|")
              case _ => print("|_ _|")
            }
            case _ => print("|_ _|")
          }
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

    val shotsGivenRemained = player.shotsGiven
      
       def printer = {
      @tailrec
      def printerHelper(x: Int, y: Int, remainedShipsCells: Set[Cell]): Unit = {
       if (x == 0 && y < SIZE) print(s"|_${y}_|")

       (x, y) match {
         case (SIZE, _) =>
          println
          printerHelper(0, y + 1, shotsGivenRemained)
         case (_, SIZE) =>
          println
        case _ => 
          val cell = shotsGivenRemained.find(c => c.x == x && c.y == y)
          cell match {
            case Some(c) => c.state match {
              case Miss => print("|_O_|")
              case Sink => print("|_X_|")
              case Hit => print("|_+_|")
              case _ => print("|_ _|")
            }
            case _ => print("|_ _|")
          }
         printerHelper(x + 1, y, shotsGivenRemained.filter(c => c.x != x || c.y != y))
       }
      }
      printerHelper(0, 0, shotsGivenRemained)
    }

    player match {
      case _: Human => println(s"${player.name} Guesses")
      case _: AI => println(s"${player.name}(AI) guesses")
    }
    printTopRow
    printer
  }
}
