import org.scalatest.FunSuite
import main.scala.battleships._
import main.scala.battleships.State._
import main.scala.battleships.Direction._
import main.scala.battleships.Board._

class FleetTest extends FunSuite {
    test("create Fleet with set of ships"){
        assert(Fleet(Set(Ship(3, 3, Vertical, 1), Ship(2, 2, Vertical, 2))).getShipsCells ==
         Set(Cell(3, 3, Occupied),Cell(2, 2, Occupied),Cell(2, 3, Occupied)))
    }
    test("hit one cell of ship(with size 2) of a fleet") {
       val x = Fleet(Set(Ship(3, 3, Vertical, 1), Ship(2, 2, Vertical, 2))).hit(Cell(2,2))
        assert(x.fleet.getShipsCells == Set(Cell(3, 3, Occupied),Cell(2, 2, Hit),Cell(2, 3, Occupied))  && x.isHit && x.sunkShip == None)
    }
    test("destroy one ship of a fleet") {
       val x = Fleet(Set(Ship(3, 3, Vertical, 1), Ship(2, 2, Vertical, 2))).hit(Cell(3,3))
        assert(x.fleet.getShipsCells == Set(Cell(3, 3, Sink),Cell(2, 2, Occupied),Cell(2, 3, Occupied))  && x.isHit && x.sunkShip == Some(Ship(Set(Cell(3, 3, Sink)))))
    }
    test("addRandomShip adds ship to a fleet") {
        assert(Fleet().addRandomShip(2).get.getShipsCoordinates != Nil)
    }
    test("isOverlapping") {
        assert(Fleet(Set(Ship(0,0,Horizontal, 1))).isOverlapping(Ship(0,0,Horizontal,1)))
    }
    test("isOverlapping with ship which is not overlapping") {
        assert(!Fleet(Set(Ship(0,0,Horizontal, 1))).isOverlapping(Ship(3,6,Horizontal,1)))
    }
    test("Fleet.isHit") {
        assert(Fleet(Set(Ship(0,0,Horizontal, 1))).isHit(Cell(0,0)))
    }
    test("Fleet.is(Not)Hit") {
        assert(!Fleet(Set(Ship(0,0,Horizontal, 1))).isHit(Cell(5,0)))
    }
    
}