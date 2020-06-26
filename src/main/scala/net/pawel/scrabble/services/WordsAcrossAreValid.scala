package net.pawel.scrabble.services

import net.pawel.scrabble.{Play, WordPlayed}

class WordsAcrossAreValid(private val wordsService: Words,
                          private val wordsAcross: WordsAcross) {

  def apply(play: Play) = wordsAreValid(play.wordsAcross)

  def apply(word: String,
            rowIndex: Int,
            columnIndex: Int,
            indicesWithinWord: List[Int]): Boolean = {
    val words = acrossWords(rowIndex, indicesWithinWord, columnIndex, word)
    wordsAreValid(words)
  }

  private def acrossWords(rowIndex: Int, indices: List[Int], columnIndex: Int, word: String) =
    wordsAcross.acrossWords(word, rowIndex, columnIndex, indices)

  private def wordsAreValid(words: List[WordPlayed]): Boolean =
    words.map(_.string()).forall(wordsService.isValid)
}
