import BattleShips._

import scala.annotation.tailrec

object Game extends App {
  @tailrec
  def game(state: gState): gState ={
    if (state.p1.isInstanceOf[Human]){
      Board.printLegend
      Board.showMyBoard(state.p1)
    }
      Board.showEnemyBoard(state.p1)

    val guessedCell = state.p1.shoot()
    val (newP2, didHit, ship) = state.p2.receiveShot(guessedCell)
    val newP1 = state.p1.afterShooting(guessedCell, didHit, ship)
    if (newP1.checkLose())
      state.copy(win = Some(newP2))
    else if (newP2.checkLose())
      state.copy(win = Some(newP2))
    else {
      game(state.copy(p1 = newP2, p2 = newP1))
    }
  }
  def initBoard(state: gState) = {
    state.copy(p1 = state.p1.placeShips(Board.shipsToPlace), p2 = state.p2.placeShips(Board.shipsToPlace))
  }
  def startGame(): Unit= {
    val ScalonyWojownik = AI()
    val Gracz = Human("Kapitan Pazur")
    val finalState: gState = game(initBoard(gState(Gracz, ScalonyWojownik, None)))
    val winner = finalState.win.get
    Board.showEnemyBoard(ScalonyWojownik)
    Board.showEnemyBoard(Gracz)
    println(s"${winner.name} won the game!")
  }
  startGame()
}