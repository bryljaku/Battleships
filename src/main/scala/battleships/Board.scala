package main.scala.battleships
import _root_.scala.annotation.tailrec
import State._
import com.typesafe.config.{Config, ConfigFactory }
import collection.JavaConversions._
object Board {
 val conf = ConfigFactory.load
  val SIZE = conf.getInt("battleships.size")
  val shipsToPlace = conf.getString("battleships.ships").split(",").toList.map(_.toInt)

  def printTopRow: Unit = {
    print("|_ _|")
    for (x <- 0 to SIZE - 2) print(s"|_${x}_|")
    println(s"|_${SIZE - 1}_|")
  }

  def printLegend: Unit = {
    println("X - Destroyed, + - Hit, O - Miss, $ - Ship")
  }

  def showMyBoard(player: Player): Unit = {
    println("My board")
    printTopRow
    printer(player.fleet.getShipsCells)
  }

  def showEnemyBoard(player: Player): Unit = {
    player match {
      case _: Human => println(s"${player.name} Guesses")
      case _: AI => println(s"${player.name}(AI) guesses")
    }
    printTopRow
    printer(player.shotsGiven)
  }

  def printer(shipsCells:Set[Cell]) = {
    @tailrec
    def printerHelper(x: Int, y: Int, cellsRemained: Set[Cell]): Unit = {
     if (x == 0 && y < SIZE) print(s"|_${y}_|")
      (x, y) match {
        case (SIZE, _) =>
        println
         printerHelper(0, y + 1, cellsRemained)
       case (_, SIZE) =>
        println
       case _ => 
         val cell = cellsRemained.find(c => c.x == x && c.y == y)
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
        printerHelper(x + 1, y, cellsRemained.filter(c => c.x != x || c.y != y))
     }
    }
    printerHelper(0, 0, shipsCells)
  }
}
