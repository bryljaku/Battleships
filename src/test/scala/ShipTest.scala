import org.scalatest.FunSuite
import main.scala.battleships._
import main.scala.battleships.State._
import main.scala.battleships.Direction._
class ShipTest extends FunSuite {
    test("Ship.apply(positions: Set[Cell])") {
        assert(Ship(Set(Cell(0,0, Occupied), Cell(1,0, Occupied), Cell(2,0, Hit), Cell(3,0, Occupied))).getCoordinates ==
         Set((0,0), (1,0), (2,0), (3,0)))
    }
    test("Ship.apply(_, _, _, _)") {
        assert(Ship(0, 0, Horizontal, 4).getCoordinates == Set((0,0), (1,0), (2,0), (3,0)))
    }
    test("Ship of size 4 is hit once") {
        assert(Ship(0,0,Horizontal, 4).hit(Cell(2,0)) == 
            Ship(Set(Cell(0,0, Occupied), Cell(1,0, Occupied), Cell(2,0, Hit), Cell(3,0, Occupied))))
    }
    test("Ship of size 4 is hit 3 times") {
        assert(Ship(0,0,Horizontal, 4).hit(Cell(0,0)).hit(Cell(1,0)).hit(Cell(2,0)) == 
            Ship(Set(Cell(0,0, Hit), Cell(1,0, Hit), Cell(2,0, Hit), Cell(3,0, Occupied))))
    }
    test("Ship of size 4 is hit 4 times and sunk") {
        assert(Ship(0,0,Horizontal, 4).hit(Cell(0,0)).hit(Cell(1,0)).hit(Cell(2,0)).hit(Cell(3,0)) == 
            Ship(Set(Cell(0,0, Sink), Cell(1,0, Sink), Cell(2,0, Sink), Cell(3,0, Sink))))
    }
    test("Ship.isSunk") {
        assert(Ship(0,0,Horizontal, 4).hit(Cell(0,0)).hit(Cell(1,0)).hit(Cell(2,0)).hit(Cell(3,0)).isSunk)
    }
    test("Ship hit once and Ship.isSunk is called") {
        assert(!Ship(Set(Cell(0,0, Occupied), Cell(1,0, Occupied), Cell(2,0, Hit), Cell(3,0, Occupied))).isSunk)
    }
    test("Ship.isTouched") {
        assert(Ship(0, 0, Horizontal, 2).isTouched(Cell(0,0)))
    }
    test("Ship.isTouched called with cell not overlapping") {
        assert(!Ship(0, 0, Horizontal, 2).isTouched(Cell(3,3)))
    }
    test("Ships overlapping each other") {
        Ship(0, 0, Horizontal, 3).isOverlapping(Ship(0, 0, Vertical, 3))
    }
    test("Ships not overlapping each other") {
        !Ship(0, 0, Horizontal, 3).isOverlapping(Ship(3, 3, Vertical, 3))
    }
}