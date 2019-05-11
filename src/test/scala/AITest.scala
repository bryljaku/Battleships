import org.scalatest.FunSuite
import main.scala.battleships._
import main.scala.battleships.State._
import main.scala.battleships.Direction._
import main.scala.battleships.Board.SIZE
class AITest extends FunSuite {

    val testAI = AI(fleet= Fleet(Set(Ship(3, 3, Vertical, 1), Ship(2, 2, Vertical, 2))), 
    shotsGiven = Set(Cell(3,3,Miss), Cell(3,4,Hit), Cell(4,4,Hit), Cell(5,4, Miss)))


    // test("placeShips") {
    //     assert(!AI().placeShips(List(1,2,3,4)).fleet.getShipsCoordinates.exists((x,y):(Int, Int) => 
    //     x < 0 || y < 0 || x >= SIZE || y >= SIZE))
    //     }

     test("shootInRow") {
         assert(testAI.shoot == Cell(2,4))
     }
     test("shootAroundIsolatedCell") {
         val x = AI(shotsGiven = Set(Cell(1,1,Hit), Cell(1,2,Miss), Cell(1,0, Miss), Cell(2,1, Miss)))
        assert(x.shoot == Cell(0,1))
     }
    test("AI randomShoot") {
        assert(AI().shoot() match {
            case Cell(x, y, _) if x < 0 && x >= SIZE && y < 0 && y>= SIZE => false
            case _ => true 
        })
    }
    test("AI afterShooting  hit") {
        assert(testAI.afterShooting(Cell(3,2,Hit), true, None) == testAI.copy(shotsGiven =
            Set(Cell(3,2,Hit),Cell(3,3,Miss), Cell(3,4,Hit), Cell(4,4,Hit), Cell(5,4, Miss))))
    }
    test("AI afterShooting  sunk ship") {
        assert(AI(shotsGiven = Set(Cell(0,0,Hit))).afterShooting(Cell(1,0,Sink),true,Some(Ship(Set(Cell(0,0,Sink),Cell(1,0,Sink))))) ==
        AI(shotsGiven = Set(Cell(0,0,Sink),Cell(1,0,Sink)), sinkShips = Set(Ship(Set(Cell(0,0,Sink), Cell(1,0,Sink))))))
        }
    test("AI aftershooting ") {

    }

}