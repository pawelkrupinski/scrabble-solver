package net.pawel.scrabble

import net.pawel.scrabble.Main.runMain

object Main {
  def main(args: Array[String]): Unit = {
    val letters = "anadoee"
    val filename = "current.txt"

    runMain(letters, filename)
  }

  def runMain(letters: String, filename: String): Unit = {
    val definition: BoardDefinition = LoadBoardDefinition()
    val board = LoadBoard(filename).getOrElse(Board())
    val words = Words.makeWords
    val game = Game(words, board, definition)
    mainLoop(game, letters)
  }

  def printOptions(options: List[ScoredPlay]) {
    options.take(40).foreach(printOption)
  }

  def printOption(scoredPlay: ScoredPlay) {
    val score = scoredPlay.score
    val play = scoredPlay.play
    val word = play.word
    val row = word.row
    val column = play.word.column
    val string = play.word.string()
    val direction = word.direction
    val words = play.wordsAcross.map(_.string()).mkString(", ")
    println(s"$score ($row, $column, $direction) $string : $words")
  }

  def mainLoop(game: Game, startLetters: String = "") = {
    var letters = startLetters

    if (game.board.isNotEmpty()) {
      printGame(game)
    }

    val options = game.sortedOptions(letters)
    printOptions(options)
  }

  private def printGame(game: Game) = {
    println(PrintBoard(game.board))
  }

  private def readLine() = Console.in.readLine()
}

case class Game(wordsService: Words = Words.makeWords(),
                board: Board = Board(),
                definition: BoardDefinition = LoadBoardDefinition()) {

  def transposed(): Game = copy(board = board.transposed(), definition = definition.transposed())

  def sortedOptions(letters: String) =
    options(letters).map(_.score(definition, board)).sortBy(_.score).reverse

  def options(letters: String): List[Play] = calculateOptions(letters) ::: transposedOptions(letters)

  private def transposedOptions(letters: String) = transposed().calculateOptions(letters).map(_.transposed())

  def calculateOptions(letters: String): List[Play] = {
    val tilesWithAdjacents = new TilesWithAdjacents(board)
    val wordsAcross = new WordsAcross(board, wordsService)
    val calculateAdjacentWords = new CalculateAdjacentWords(this, tilesWithAdjacents, wordsAcross)
    val calculateCrossingWords = new CalculateCrossingWords(this, wordsService, wordsAcross, tilesWithAdjacents)

    val words = wordsService.wordsSpelledBy(letters)
    (0 to 14).flatMap(rowIndex => {
      calculateAdjacentWords.adjacentWordOptions(words, rowIndex, 7) ++
        calculateCrossingWords.calculateCrossingWords(letters, rowIndex)
    }).toList
  }


  def update(word: String, rowIndex: Int, columnIndex: Int): Game = {
    val tiles = Tiles.tiles(word)
    copy(board = board.add(tiles, rowIndex, columnIndex))
  }
}
