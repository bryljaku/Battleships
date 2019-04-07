package tests
import BattleShips._
object test{
// Ship
  val s = new Ship(Set(Cell(1, 2, State.Occupied), Cell(1,3, State.Hit)))
  val x = new Ship(Set(Cell(2, 2, State.Occupied), Cell(2, 3, State.Hit)))
//  println(s.positions)
//  println(s.getCoordinates)
//  println(s.isTouched(Cell(1, 2, State.Hit)))
//  println(s.isTouched(Cell(1, 9, State.Hit)))
//  println(s.isOverlapping(s))
//  println(s.isOverlapping(x))
  var k = Ship(1, 2, 'H', 4)
//  println(k.getCoordinates)
//  println(k.positions)
//  k = k.hit(Cell(1,2,State.Occupied))
//  k = k.hit(Cell(4,2,State.Occupied))
//  k = k.hit(Cell(3,2,State.Occupied))
//  println(k.positions)
//  k = k.hit(Cell(2,2,State.Occupied))
//  println(k.positions)
//Fleet
  var fleet = new Fleet(Set(k,s,x))
//  var heh = fleet.addShip(k)


  println(fleet.getShipsCells)
  println(fleet.getShipsCoordinates)
//Player
//class play(val name: String,val fleet: Fleet, val shotsGiven: Set[Cell], val shotsReceived: Set[Cell], val sinkShips: Set[Ship]) extends Player{
//  override def placeShips(lengths: List[Int]): Player = new play(name, fleet, shotsGiven, shotsReceived, sinkShips)
//
//  override def guess(cell: Cell): Cell = Cell(1,1,State.Hit)
//
//  override def receiveShot(cell: Cell): Unit = println()
//
//}
//  val gracz = new play("kak", fleet, Set(), Set(), Set())
//  Board.showMyBoard(gracz)
}
