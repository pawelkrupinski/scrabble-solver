package net.pawel.scrabble.services

import net.pawel.scrabble.{Across, Game, Play, Word, WordPlayed}

class CalculateInitialWord(val game: Game,
                           val wordService: Words) {

  val Index_Of_The_Middle_Tile = 7

  def apply(letters: String): Iterator[Play] = {
    val words = wordService.iteratorWordsSpelledBy(letters)

    for (word <- words;
         index <- 0 to word.length - 1 if wordFitsOnTheBoard(word, index))
      yield {
        val row = Index_Of_The_Middle_Tile
        val column = Index_Of_The_Middle_Tile - index
        val wordPlayed = WordPlayed(row, column, Word(word), Across, allLettersUsed(word))

        Play(game.update(word, row, column), wordPlayed, Nil)
      }
  }

  private def allLettersUsed(word: String) = word.length == 7

  private def wordFitsOnTheBoard(word: String, index: Int) = {
    val doesNotOverflowToTheRight = Index_Of_The_Middle_Tile + word.length - index < 15
    val doesNotOverflowToTheLeft = Index_Of_The_Middle_Tile - index >= 0
    doesNotOverflowToTheRight && doesNotOverflowToTheLeft
  }
}
