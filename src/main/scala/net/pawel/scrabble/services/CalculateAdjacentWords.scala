package net.pawel.scrabble.services

import net.pawel.scrabble._

class CalculateAdjacentWords(val game: Game,
                             val wordsService: Words,
                             val tilesWithAdjacents: TilesWithAdjacents,
                             val wordsAcross: WordsAcross,
                             val wordsAcrossAreValid: WordsAcrossAreValid) {
  val Letters_For_Bonus = 7
  val board = game.board

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

  private def wordInARow(word: String, rowIndex: Int, indices: List[Int]) = {
    val firstIndex = indices.head
    val lastIndex = indices.last
    val length = word.length;
    val start = scala.math.max(firstIndex - length + 1, 0);
    val end = scala.math.min(lastIndex, 14 - length + 1);
    for (columnIndex <- (start to end).iterator;
         result <- tryWord(word, rowIndex, columnIndex).toList)
      yield result
  }

  private def doesNotExceedLengthOfTheRow(lastIndex: Int) = lastIndex < 15

  private def coversAnIndexThatHasAdjacents(rowIndex: Int,
                                            firstColumnIndex: Int,
                                            lastColumnIndex: Int) = {
    val indices = tilesWithAdjacents.indices(rowIndex)
    indices.exists(i => firstColumnIndex <= i && i <= lastColumnIndex)
  }

  private def doesNotCoverPlacedTiles(index: Int,
                                      lastIndex: Int,
                                      rowIndex: Int): Boolean = {
    val row = game.board.row(rowIndex)
    !(index to lastIndex).exists(row(_).isDefined)
  }

  private def doesNotTouchPlacedTilesInRow(index: Int,
                                           lastIndex: Int,
                                           rowIndex: Int): Boolean = {
    val row = board.row(rowIndex)
    (index == 0 || row(index - 1).isEmpty) && (lastIndex == 14 || row(lastIndex + 1).isEmpty)
  }

  private def formsValidWords(word: String, rowIndex: Int, columnIndex: Int) = {
    wordsAcrossAreValid(word, rowIndex, columnIndex)
  }

  private def calculateWordsAcross(word: String,
                                   rowIndex: Int,
                                   columnIndex: Int): List[WordPlayed] = {
    val indices = tilesWithAdjacents.indicesWithinWord(rowIndex, columnIndex, word)

    def letterAt(i: Int): Char = word(i - columnIndex)

    val words = indices.map(columnIndex => wordsAcross.acrossWord(rowIndex, columnIndex,
      letterAt(columnIndex)))
    words
  }

  private def tryWord(word: String,
                      rowIndex: Int,
                      columnIndex: Int) = {
    val lastColumnIndex = columnIndex + word.length - 1
    if (doesNotExceedLengthOfTheRow(lastColumnIndex) &&
      coversAnIndexThatHasAdjacents(rowIndex, columnIndex, lastColumnIndex) &&
      doesNotCoverPlacedTiles(columnIndex, lastColumnIndex, rowIndex) &&
      doesNotTouchPlacedTilesInRow(columnIndex, lastColumnIndex, rowIndex) &&
      formsValidWords(word, rowIndex, columnIndex)) {
      val allLettersUsed = word.length == Letters_For_Bonus
      val wordPlayed = WordPlayed(rowIndex, columnIndex, Word(word), Across, allLettersUsed)
      val words = calculateWordsAcross(word, rowIndex, columnIndex)
      val newGame = game.update(word, rowIndex, columnIndex)
      Some(Play(newGame, wordPlayed, words))
    } else None
  }
}
