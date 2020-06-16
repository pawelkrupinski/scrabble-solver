package net.pawel.scrabble

class WordsAcross(val board: Board, wordsService: Words) {
  def acrossWordsAreValid(word: String, rowIndex: Int, columnIndex: Int, indicesWithinWord: List[Int]) = {
    val words = acrossWords(word, rowIndex, columnIndex, indicesWithinWord)
    words.map(_.string()).forall(wordsService.isValid)
  }

  def acrossWords(word: String, rowIndex: Int, columnIndex: Int, indicesWithinWord: List[Int]) = {
    def letterAt(i: Int) = word(i - columnIndex)

    indicesWithinWord.map(index => acrossWord(rowIndex, index, letterAt(index)))
  }

  def acrossWordIsValid(rowIndex: Int, columnIndex: Int, char: Char): Boolean = {
    val word: WordPlayed = acrossWord(rowIndex, columnIndex, char)
    wordsService.isValid(word.string())
  }

  def acrossWord(rowIndex: Int, columnIndex: Int, char: Char) = {
    val column = board.column(columnIndex)
    val word = lettersBefore(column, rowIndex - 1, columnIndex) + char + lettersAfter(column, rowIndex + 1)
    word
  }

  def lettersBefore(column: List[Option[Tile]], rowIndex: Int, columnIndex: Int, result: String = ""): WordPlayed =
    if (rowIndex < 0 || column(rowIndex).isEmpty)
      WordPlayed(rowIndex + 1, columnIndex, Word(result), Down)
    else lettersBefore(column, rowIndex - 1, columnIndex, column(rowIndex).get.letter + result)

  def lettersAfter(column: List[Option[Tile]], rowIndex: Int, result: String = ""): String =
    if (rowIndex > 14 || column(rowIndex).isEmpty) result
    else lettersAfter(column, rowIndex + 1, result + column(rowIndex).get.letter)

}
