package main.scala.battleships

import scala.annotation.tailrec

object Game extends App {
  @tailrec
  def game(state: GameState): GameState ={
    
      Board.printLegend
      Board.showMyBoard(state.player1)
      Board.showEnemyBoard(state.player1)

    val guessedCell = state.player1.shoot()
    val receiveShotValue = state.player2.receiveShot(guessedCell)
    val newPlayer2 = receiveShotValue.player

    val newPlayer1 = state.player1.afterShooting(guessedCell, receiveShotValue.isHit, receiveShotValue.sunkShip)
    
    if (newPlayer2.checkLose())
      state.copy(player1 = newPlayer2, player2 = newPlayer1, win = Some(newPlayer2))
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
    Board.showEnemyBoard(finalState.player2)
    Board.showEnemyBoard(finalState.player1)
    val winner = finalState.win match {
      case Some(player) => player.name
      case _ => "Something went wrong."
    }
    println(s"${winner} won the game!")
  }
  startGame()
}
