import org.scalatest.FunSuite
import main.scala.battleships._
import main.scala.battleships.State._
import main.scala.battleships.Direction._
import main.scala.battleships.Board.SIZE
class AITest extends FunSuite {

     test("shootInRow") {
         assert(AI(shotsGiven = Set(Cell(3,3,Miss), Cell(2,4,Hit), Cell(3,4,Hit), Cell(4,4,Hit), Cell(5,4, Miss))).shoot == Cell(1,4))
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
        assert(AI(shotsGiven = Set(Cell(5,4,Miss))).afterShooting(Cell(3,2,Hit), true, None) == AI(shotsGiven = Set(Cell(3,2,Hit), Cell(5,4,Miss))))
    }
    test("AI afterShooting  sunk ship") {
        assert(AI(shotsGiven = Set(Cell(0,0,Hit))).afterShooting(Cell(1,0,Sink),true,Some(Ship(Set(Cell(0,0,Sink),Cell(1,0,Sink))))) ==
        AI(shotsGiven = Set(Cell(0,0,Sink),Cell(1,0,Sink)), sinkShips = Set(Ship(Set(Cell(0,0,Sink), Cell(1,0,Sink))))))
        }
        
        val receiveShotAITestObject = AI(fleet = Fleet(Set(Ship(Set(Cell(3,3,Occupied))), Ship(Set(Cell(1,2,Occupied), Cell(0,2, Occupied))))))
    test("AI receiveShot hit"){
        val receiveShotAITestObjectHit =  AI(fleet = Fleet(Set(Ship(Set(Cell(3,3,Occupied))), Ship(Set(Cell(1,2,Hit), Cell(0,2, Occupied))))), shotsReceived = Set(Cell(1,2,Hit)))
        val x = receiveShotAITestObject.receiveShot(Cell(1,2))
        assert(receiveShotAITestObjectHit == x.player && x.isHit && x.sunkShip == None)
    }
    test("AI receiveShot sunk"){
        val receiveShotAITestObjectSunk =  AI(fleet = Fleet(Set(Ship(Set(Cell(3,3,Sink))), Ship(Set(Cell(1,2,Occupied), Cell(0,2, Occupied))))), shotsReceived = Set(Cell(3,3,Sink)))
        val x = receiveShotAITestObject.receiveShot(Cell(3,3))
        assert(receiveShotAITestObjectSunk == x.player && x.isHit && x.sunkShip == Some(Ship(Set(Cell(3,3,Sink)))))
    }
    test("AI receiveShot miss"){
        val x = receiveShotAITestObject.receiveShot(Cell(4,4))    
        assert(receiveShotAITestObject.copy(shotsReceived = Set(Cell(4,4,Miss))) == x.player && !x.isHit && x.sunkShip == None)
    }

}