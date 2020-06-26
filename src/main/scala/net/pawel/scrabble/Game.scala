package net.pawel.scrabble

import net.pawel.scrabble.load.{LoadBoard, LoadBoardDefinition, PrintBoard, SaveBoard}
import net.pawel.scrabble.services.{CalculateAdjacentWords, CalculateCrossingWords, CalculateInitialWord, CreatePlay, Tiles, TilesWithAdjacents, Words, WordsAcross, WordsAcrossAreValid}

import scala.io.StdIn

case class Game(board: Board = Board(),
                definition: BoardDefinition = LoadBoardDefinition(),
                wordsService: Words = Words.makeWords()) {

  def transposed(): Game = copy(board = board.transposed(), definition = definition.transposed())

  def sortedOptions(letters: String) =
    options(letters).map(_.score(definition, board)).toList.sortBy(_.score).reverse

  def options(letters: String): Iterator[Play] = calculateOptions(letters) ++ transposedOptions(letters)

  private def transposedOptions(letters: String) = transposed().calculateOptions(letters).map(_.transposed())

  def calculateOptions(letters: String): Iterator[Play] = {
    if (board.isEmpty()) {
      val calculateInitialWord = new CalculateInitialWord(this, wordsService)

      calculateInitialWord(letters)
    } else {
      val tilesWithAdjacents = new TilesWithAdjacents(board)
      val wordsAcross = new WordsAcross(board, wordsService)
      val wordsAcrossAreValid = new WordsAcrossAreValid(wordsService, wordsAcross)
      val calculateAdjacentWords = new CalculateAdjacentWords(this, wordsService, tilesWithAdjacents, wordsAcross, wordsAcrossAreValid)
      val createPlay = new CreatePlay(this, tilesWithAdjacents, wordsAcross)
      val calculateCrossingWords = new CalculateCrossingWords(this, wordsService, wordsAcrossAreValid, createPlay)

      calculateAdjacentWords(letters) ++ calculateCrossingWords(letters)
    }
  }

  def update(word: String, rowIndex: Int, columnIndex: Int): Game = {
    val tiles = Tiles.tiles(word)
    copy(board = board.add(tiles, rowIndex, columnIndex))
  }
}
