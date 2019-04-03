package BattleShips


//import scala.collection.mutable.MutableList
class Board(private val board: List[List[Cell]]) {

}
object Board {
  def apply(size: Int): Board = new Board(List.fill(size)[List.fill(size)[Cell]])
}