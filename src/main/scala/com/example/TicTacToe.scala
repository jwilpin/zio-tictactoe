package com.example

import com.example.domain.*
import zio.*

import java.io.IOException

object TicTacToe extends ZIOAppDefault:

  def run =
    for
      playerPiece        <- choosePlayerPiece
      pieceThatGoesFirst <- whichPieceGoesFirst
      whoIsCross          = if playerPiece == Piece.X then Player.Human else Player.Computer
      initialState       <- ZIO.succeed(State.Ongoing(Board.empty, whoIsCross = whoIsCross, turn = pieceThatGoesFirst))
      _                  <- programLoop(initialState)
    yield ()

  val choosePlayerPiece: IO[IOException, Piece] =
    for
      in    <- Console.readLine("deseas X o O?")
      piece <- ZIO.from(Piece.make(in)).orElse(Console.printLine("entrada invalida") <*> choosePlayerPiece)
    yield piece

  def whichPieceGoesFirst: UIO[Piece] = Random.nextBoolean.map {
    case true  => Piece.X
    case false => Piece.O
  }

  def programLoop(state: State): IO[IOException, Unit] = state match
    case s @ State.Ongoing(board, whoIsCross, turn) =>
      drawBoard(board) <*> step(s).flatMap(programLoop)
    case State.Over(board)                          =>
      drawBoard(board)

  def drawBoard(board: Board): IO[IOException, Unit] =
    Console.printLine {
      Field.values
        .map(field => board.fields.get(field) -> field.ordinal)
        .map {
          case (Some(piece), _) => piece.toString
          case (None, value)    => value.toString
        }
        .sliding(3, 3)
        .map(fields => s""" ${fields.mkString(" ║ ")} """)
        .mkString("\n═══╬═══╬═══\n")
    }

  def step(state: State.Ongoing): IO[IOException, State] =
    for
      nextMove  <- if state.isComputerTurn then getComputerMove(state.board) else getPlayerMove(state.board)
      nextState <- takeField(state, nextMove)
    yield nextState

  def getComputerMove(board: Board): IO[IOException, Field] =
    for
      randomFields <- Random.shuffle(board.unoccupiedFields)
      randomField  <- ZIO.from(randomFields.headOption).orDieWith(_ => new IllegalStateException("invalid state"))
      _            <- Console.readLine("esperando por el movimiento de la compu")
    yield randomField

  def getPlayerMove(board: Board): IO[IOException, Field] =
    for
      input    <- Console.readLine("cual es tu movimiento 0-8")
      tmpField <- ZIO.from(Field.make(input)) <> (Console.printLine("input invalido") <*> getPlayerMove(board))
      field    <-
        if board.fieldIsNotFree(tmpField)
        then Console.printLine("campo ocupado") <*> getPlayerMove(board)
        else ZIO.succeed(tmpField)
    yield field

  def takeField(state: State.Ongoing, field: Field): IO[IOException, State] =
    for
      updatedBoard <- ZIO.succeed(state.board.updated(field, state.turn))
      updatedTurn  <- ZIO.succeed(state.turn.next)
      gameResult   <- getGameResult(updatedBoard)
      nextState    <- gameResult.map { result =>
                        Console.printLine(result.show).as(State.Over(updatedBoard))
                      }.getOrElse {
                        ZIO.succeed(state.copy(board = updatedBoard, turn = updatedTurn))
                      }
    yield nextState

  def getGameResult(board: Board): UIO[Option[GameResult]] =
    for
      crossWin   <- isWinner(board, Piece.X)
      noughtWin  <- isWinner(board, Piece.O)
      gameResult <-
        if crossWin && noughtWin then
          ZIO.die(new IllegalStateException("It should not be possible for both players to win!"))
        else if crossWin then ZIO.succeed(GameResult.Win(Piece.X)).asSome
        else if noughtWin then ZIO.succeed(GameResult.Win(Piece.O)).asSome
        else if board.isFull then ZIO.succeed(GameResult.Draw).asSome
        else ZIO.none
    yield gameResult

  def isWinner(board: Board, piece: Piece): UIO[Boolean] =
    Board.winnerCombinations.map(combinations => combinations.exists(_ subsetOf board.fieldsOccupiedByPiece(piece)))
