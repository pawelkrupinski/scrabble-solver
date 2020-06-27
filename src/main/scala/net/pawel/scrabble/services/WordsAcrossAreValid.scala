package net.pawel.scrabble.services

import net.pawel.scrabble.{Play, WordPlayed}



class WordsAcrossAreValid(private val wordsService: Words,
                          private val wordsAcross: WordsAcross) {

  def apply(play: Play): Boolean = wordsAreValid(play.wordsAcross)

  def apply(word: String, rowIndex: Int, columnIndex: Int): Boolean = {
    val words = acrossWords(rowIndex, columnIndex, word)
    wordsAreValid(words)
  }

  private def acrossWords(rowIndex: Int, columnIndex: Int, word: String): List[WordPlayed] =
    wordsAcross.acrossWords(word, rowIndex, columnIndex)

  private def wordsAreValid(words: List[WordPlayed]): Boolean =
    words.map(_.string()).forall(wordsService.isValid)
}
