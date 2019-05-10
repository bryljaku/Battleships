import org.scalatest.FunSuite
import main.scala.battleships._
import main.scala.battleships.State._
import main.scala.battleships.Direction._
class FleetTest extends FunSuite {
    //     test("Ship.apply(positions: Set[Cell])") {
    //     assert(Ship(Set(Cell(0,0, Occupied), Cell(1,0, Occupied), Cell(2,0, Hit), Cell(3,0, Occupied))).getCoordinates ==
    //      Set((0,0), (1,0), (2,0), (3,0)))
    // }
    test("create Fleet with set of ships"){
        assert(Fleet(Set(Ship(3, 3, Vertical, 1), Ship(2, 2, Vertical, 2))).getShipsCells == Set(Cell(3, 3, Occupied),Cell(2, 2, Occupied),Cell(2, 3, Occupied)))
    }

    test("hit one cell of ship(with size 2) of a fleet") {
       val x = Fleet(Set(Ship(3, 3, Vertical, 1), Ship(2, 2, Vertical, 2))).hit(Cell(2,2))
        assert(x._1.getShipsCells == Set(Cell(3, 3, Occupied),Cell(2, 2, Hit),Cell(2, 3, Occupied))  && x._2 && x._3 == None)
    }
    test("destroy one ship of a fleet") {
       val x = Fleet(Set(Ship(3, 3, Vertical, 1), Ship(2, 2, Vertical, 2))).hit(Cell(3,3))
        assert(x._1.getShipsCells == Set(Cell(3, 3, Sink),Cell(2, 2, Occupied),Cell(2, 3, Occupied))  && x._2 && x._3 == Some(Ship(Set(Cell(3, 3, Sink)))))
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
    // test("addRandomShip makes ship which is in boundaries") {
    //     assert(Fleet().addRandomShip(2).get.getShipsCoordinates.find(c => c.x < 0 || c.y < 0 || c.x > BOARD.SIZE || c.x > BOARD.SIZE))
    // }
    
}