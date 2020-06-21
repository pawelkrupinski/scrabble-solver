package net.pawel.scrabble.services

import net.pawel.scrabble.{Game, Play, WordPlayed}

class WordsAcrossAreValid(private val game: Game,
                          private val wordsService: Words,
                          private val wordsAcross: WordsAcross,
                          private val tilesWithAdjacents: TilesWithAdjacents) {

  def apply(wordPlayed: WordPlayed): IterableOnce[Play] = {
    val rowIndex = wordPlayed.row
    val indices = tilesWithAdjacents.tilesThatHaveAdjacentsWithinWord(rowIndex, wordPlayed)
    val columnIndex = wordPlayed.column
    val word = wordPlayed.string()
    val acrossWords = wordsAcross.acrossWords(word, rowIndex, columnIndex, indices)
    if (wordsAreValid(acrossWords)) {
      val play = Play(game.update(word, rowIndex, columnIndex), wordPlayed, acrossWords)
      List(play)
    } else Nil
  }

  private def wordsAreValid(words: List[WordPlayed]): Boolean = words.map(_.string()).forall(wordsService.isValid)
}
