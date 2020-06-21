package net.pawel.scrabble.services

import net.pawel.scrabble._

class CalculateAdjacentWords(val game: Game,
                             val tilesWithAdjacents: TilesWithAdjacents,
                             val wordsAcross: WordsAcross) {

  val Letters_For_Bonus = 7

  private def doesNotExceedLengthOfTheRow(lastIndex: Int) = lastIndex < 15

  private def coversAnIndexThatHasAdjacents(index: Int, lastIndex: Int, indicesThatHaveAdjacents: List[Int]) =
    indicesThatHaveAdjacents.exists(i => index <= i && i <= lastIndex)

  private def doesNotCoverPlacedTiles(index: Int, lastIndex: Int, row: List[Option[Tile]]): Boolean =
    !(index to lastIndex).exists(row(_).isDefined)

  private def doesNotTouchPlacedTilesInRow(index: Int, lastIndex: Int, row: List[Option[Tile]]): Boolean =
    (index == 0 || row(index - 1).isEmpty) && (lastIndex == 14 || row(lastIndex + 1).isEmpty)

  private def formsValidWords(word: String,
                      rowIndex: Int,
                      columnIndex: Int,
                      lastColumnIndex: Int,
                      indicesThatHaveAdjacents: List[Int]): Boolean = {
    val indices = tilesWithAdjacents.indicesWithinWord(columnIndex, lastColumnIndex,
      indicesThatHaveAdjacents)

    wordsAcross.acrossWordsAreValid(word, rowIndex, columnIndex, indices)
  }

  private def calculateWordsAcross(word: String,
                          rowIndex: Int,
                          columnIndex: Int,
                          lastColumnIndex: Int,
                          indicesThatHaveAdjacents: List[Int]) = {
    val indices = tilesWithAdjacents.indicesWithinWord(columnIndex, lastColumnIndex,
      indicesThatHaveAdjacents)

    def letterAt(i: Int): Char = word(i - columnIndex)

    val words = indices.map(columnIndex => wordsAcross.acrossWord(rowIndex, columnIndex,
      letterAt(columnIndex)))
    words
  }

  private def tryWord(word: String,
                      rowIndex: Int,
                      columnIndex: Int,
                      row: List[Option[Tile]],
                      indicesThatHaveAdjacents: List[Int]): Option[Play] = {
    val lastColumnIndex = columnIndex + word.length - 1
    if (doesNotExceedLengthOfTheRow(lastColumnIndex) &&
      coversAnIndexThatHasAdjacents(columnIndex, lastColumnIndex, indicesThatHaveAdjacents) &&
      doesNotCoverPlacedTiles(columnIndex, lastColumnIndex, row) &&
      doesNotTouchPlacedTilesInRow(columnIndex, lastColumnIndex, row) &&
      formsValidWords(word, rowIndex, columnIndex, lastColumnIndex, indicesThatHaveAdjacents)) {
      val allLettersUsed = word.length == Letters_For_Bonus
      val wordPlayed = WordPlayed(rowIndex, columnIndex, Word(word), Across, allLettersUsed)
      val words = calculateWordsAcross(word, rowIndex, columnIndex, lastColumnIndex,
        indicesThatHaveAdjacents)
      val newGame = game.update(word, rowIndex, columnIndex)
      Some(Play(newGame, wordPlayed, words))
    } else None
  }

  def adjacentWordOptions(words: List[String], rowIndex: Int) = {
    val row = game.board.row(rowIndex)
    val indices = tilesWithAdjacents.indicesOfCellsThatHaveAdjacents(rowIndex)
    if (indices.isEmpty) Nil
    else {
      val firstIndex = indices.head
      val lastIndex = indices.last
      for (word <- words;
           length = word.length;
           start = scala.math.max(firstIndex - length + 1, 0);
           end = scala.math.min(lastIndex, 14 - length + 1);
           index <- (start to end).toList;
           result <- tryWord(word, rowIndex, index, row, indices).toList)
        yield result
    }
  }
}
