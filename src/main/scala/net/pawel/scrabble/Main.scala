package net.pawel.scrabble

import net.pawel.scrabble.load.{LoadBoard, LoadBoardDefinition, PrintBoard, SaveBoard}
import net.pawel.scrabble.services.Words

import scala.io.StdIn

object Main {
  def runMain(filename: String): Unit = {
    val definition: BoardDefinition = LoadBoardDefinition()
    val board = LoadBoard(filename).getOrElse(Board())
    val game = Game(board, definition = definition)

    val theirGame = if (board.isNotEmpty()) handleTheir(game) else game

    printGame(theirGame)

    val my = myLetters(filename)
    val options = theirGame.sortedOptions(my)
    printOptions(options)

    print("Choose play: ")
    val read = readLine()
    if (read.startsWith("r")) {
      Words.remove(read.split(" ")(1))
      return
    }

    val yourGame = options(read.toInt - 1).play.game
    printGame(yourGame)

    print("Enter your letters: ")
    val yourNewLetters = StdIn.readLine()
    SaveBoard(yourNewLetters, yourGame.board, filename)

    printOptions(yourGame.sortedOptions(yourNewLetters))
  }

  def theirLetters() = {
    print("Enter their letters: ")
    Some(StdIn.readLine()).filterNot(_.isEmpty).map(_.split(" ")).map {
      case Array(string) => Array.fill(2)(string)
      case array => array
    }
  }

  def chooseOption(options: List[ScoredPlay]): Option[Game] = {
    print("Choose play: ")
    val read = readLine()
    if (read.isEmpty) None
    else Some(options(read.toInt - 1).play.game)
  }

  private def handleTheir(game: Game): Game = {
    val their = theirLetters()

    if (their.isEmpty) {
      return game
    }

    val Some(Array(letters, word)) = their
    val options = game.sortedOptions(letters).filter(_.play.word.string() == word)
    if (options.size == 1) {
      options.head.play.game
    } else {
      printOptions(options)
      chooseOption(options).getOrElse(game)
    }
  }

  private def myLetters(filename: String) = {
    val maybeLetters = LoadBoard.loadLetters(filename)
    maybeLetters.foreach(letters => println(s"Current letters: '$letters'"))
    print("Enter your letters: ")
    StdIn.readLine() match {
      case "" => maybeLetters.get
      case letters => letters
    }
  }

  def printOptions(options: List[ScoredPlay]) {
    options.zipWithIndex.reverse.foreach {
      case (play, index) => printOption(play, index + 1)
    }
  }

  def printOption(scoredPlay: ScoredPlay, index: Int) {
    val ScoredPlay(play, score) = scoredPlay
    val word = play.word
    val row = word.row
    val column = word.column
    val direction = word.direction
    val wordAsString = word.string()
    val words = play.wordsAcross.map(_.string()).mkString(", ")
    println(s"$index. $score ($row, $column, $direction) $wordAsString : $words")
  }

  private def printGame(game: Game) = {
    println(PrintBoard(game.board))
  }

  private def readLine() = Console.in.readLine()
}
