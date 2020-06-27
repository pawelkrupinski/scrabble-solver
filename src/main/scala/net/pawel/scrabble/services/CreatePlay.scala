package net.pawel.scrabble.services

import net.pawel.scrabble.{Game, Play, WordPlayed}

class CreatePlay(private val game: Game,
                 private val wordsAcross: WordsAcross) {

  def fromWord(wordPlayed: WordPlayed) = {
    val rowIndex = wordPlayed.row
    val columnIndex = wordPlayed.column
    val word = wordPlayed.string()
    val words = wordsAcross.acrossWords(word, rowIndex, columnIndex)
    Play(game.update(word, rowIndex, columnIndex), wordPlayed, words)
  }
}
