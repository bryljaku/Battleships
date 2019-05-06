package main.scala.battleships

import scala.annotation.tailrec

object Game extends App {
  @tailrec
  def game(state: GameState): GameState ={
    if (state.player1.isInstanceOf[Human]){
      Board.printLegend
      Board.showMyBoard(state.player1)
    }
      Board.showEnemyBoard(state.player1)

    val guessedCell = state.player1.shoot()
    val (newPlayer2, didHit, ship) = state.player2.receiveShot(guessedCell)
    val newPlayer1 = state.player1.afterShooting(guessedCell, didHit, ship)

    if (newPlayer1.checkLose())
      state.copy(player1 = newPlayer2, player2 = newPlayer1, win = Some(newPlayer2))
    else if (newPlayer2.checkLose())
      state.copy(player1 = newPlayer2, player2 = newPlayer1, win = Some(newPlayer1))
    else {
      game(state.copy(player1 = newPlayer2, player2 = newPlayer1))
    }
  }
  def initBoard(state: GameState) = {
    state.copy(player1 = state.player1.placeShips(Board.shipsToPlace), player2 = state.player2.placeShips(Board.shipsToPlace))
  }
  def startGame(): Unit= {
    val computer = AI()
    val human = Human()
    val finalState: GameState = game(initBoard(GameState(human, computer, None)))
    val winner = finalState.win.get
    Board.showEnemyBoard(finalState.player2)
    Board.showEnemyBoard(finalState.player1)
    println(s"${winner.name} won the game!")
  }
  startGame()
}
