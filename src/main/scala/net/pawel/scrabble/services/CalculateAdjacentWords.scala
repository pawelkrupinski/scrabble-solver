package net.pawel.scrabble.services

import net.pawel.scrabble._

class CalculateAdjacentWords(val game: Game,
                             val wordsService: Words,
                             val tilesWithAdjacents: TilesWithAdjacents,
                             val wordsAcross: WordsAcross,
                             val wordsAcrossAreValid: WordsAcrossAreValid) {

  def apply(letters: String): Iterator[Play] = {
    val words = wordsService.wordsSpelledBy(letters)
    for (rowIndex <- (0 to 14).iterator;
         plays <- adjacentWordOptions(words.iterator, rowIndex))
      yield plays
  }

  private def adjacentWordOptions(words: Iterator[String], rowIndex: Int): Iterator[Play] = {
    val indices = tilesWithAdjacents.indices(rowIndex)

    if (indices.isEmpty)
      Iterator.empty
    else
      for (word <- words;
           play <- wordInARow(word, rowIndex, indices)) yield
        play
  }

  private def wordInARow(word: String, rowIndex: Int, indices: List[Int]): Iterator[Play] = {
    val firstIndex = indices.head
    val lastIndex = indices.last
    val length = word.length
    val start = scala.math.max(firstIndex - length + 1, 0)
    val end = scala.math.min(lastIndex, 14 - length + 1)

    for (columnIndex <- (start to end).iterator;
         result <- validateFor(word, rowIndex, columnIndex)().toList)
      yield result
  }

  private def validateFor(word: String, rowIndex: Int, columnIndex: Int) =
    new ValidateAdjacentWord(word, rowIndex, columnIndex, game, tilesWithAdjacents,
      wordsAcross, wordsAcrossAreValid)

  class ValidateAdjacentWord(private val word: String,
                             private val rowIndex: Int,
                             private val columnIndex: Int,
                             private val game: Game,
                             private val tilesWithAdjacents: TilesWithAdjacents,
                             private val wordsAcross: WordsAcross,
                             private val wordsAcrossAreValid: WordsAcrossAreValid) {
    val Letters_For_Bonus = 7
    val row = game.board.row(rowIndex)
    val lastColumnIndex = columnIndex + word.length - 1

    def apply(): Option[Play] = {
      if (isValid()) {
        val allLettersUsed = word.length == Letters_For_Bonus
        val wordPlayed = WordPlayed(rowIndex, columnIndex, Word(word), Across, allLettersUsed)
        val words = wordsAcross.acrossWords(word, rowIndex, columnIndex)
        val newGame = game.update(word, rowIndex, columnIndex)
        Some(Play(newGame, wordPlayed, words))
      } else None
    }

    private def isValid() =
      doesNotExceedLengthOfTheRow() &&
        coversAnIndexThatHasAdjacents() &&
        doesNotCoverPlacedTiles() &&
        doesNotTouchPlacedTilesInRow() &&
        formsValidWords()

    private def doesNotExceedLengthOfTheRow() = lastColumnIndex < 15

    private def coversAnIndexThatHasAdjacents() = {
      val indices = tilesWithAdjacents.indices(rowIndex)
      indices.exists(i => columnIndex <= i && i <= lastColumnIndex)
    }

    private def doesNotCoverPlacedTiles() = {
      val row = game.board.row(rowIndex)
      !(columnIndex to lastColumnIndex).exists(row(_).isDefined)
    }

    private def doesNotTouchPlacedTilesInRow() =
      (columnIndex == 0 || row(columnIndex - 1).isEmpty) &&
        (lastColumnIndex == 14 || row(lastColumnIndex + 1).isEmpty)

    private def formsValidWords() = wordsAcrossAreValid(word, rowIndex, columnIndex)
  }
}


