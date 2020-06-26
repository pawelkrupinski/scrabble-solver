package net.pawel.scrabble.services

import net.pawel.scrabble._

class WordsAcross(val board: Board, wordsService: Words) {

  def acrossWords(word: String, rowIndex: Int, columnIndex: Int,
                  indicesWithinWord: List[Int]) = {
    def letterAt(i: Int) = word(i - columnIndex)

    indicesWithinWord.map(index => acrossWord(rowIndex, index, letterAt(index)))
  }

  def acrossWord(rowIndex: Int, columnIndex: Int, char: Char) = {
    val column = board.column(columnIndex)
    val word = lettersBefore(column, rowIndex - 1, columnIndex) + char + lettersAfter(column, rowIndex + 1)
    word
  }

  private def lettersBefore(column: List[Option[Tile]], rowIndex: Int,
                    columnIndex: Int, result: String = ""): WordPlayed =
    if (rowIndex < 0 || column(rowIndex).isEmpty)
      WordPlayed(rowIndex + 1, columnIndex, Word(result), Down)
    else lettersBefore(column, rowIndex - 1, columnIndex, column(rowIndex).get.letter + result)

  private def lettersAfter(column: List[Option[Tile]], rowIndex: Int, result: String = ""): String =
    if (rowIndex > 14 || column(rowIndex).isEmpty) result
    else lettersAfter(column, rowIndex + 1, result + column(rowIndex).get.letter)

}
