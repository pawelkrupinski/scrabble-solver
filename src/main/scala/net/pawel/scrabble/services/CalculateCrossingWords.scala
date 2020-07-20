package net.pawel.scrabble.services

import net.pawel.scrabble.{Game, Play, Tile}

case class Range(startIndex: Int, endIndex: Int, tiles: List[Tile])

class CalculateCrossingWords(private val game: Game,
                             private val wordsService: Words,
                             private val wordsAcrossAreValid: WordsAcrossAreValid,
                             private val createPlay: CreatePlay) {

  def apply(letters: String): Iterator[Play] =
    (0 to 14).iterator.flatMap(calculateCrossingWords(letters, _))

  private def calculateCrossingWords(letters: String, rowIndex: Int): Iterator[Play] = {
    val row = rowAt(rowIndex)
    val ranges = FindRanges(row)

    rangesCombinations(ranges, rowIndex, row, letters)
  }

  private def rowAt(index: Int) = game.board.row(index)

  private def rangesCombinations(ranges: List[Range],
                                 rowIndex: Int,
                                 row: List[Option[Tile]],
                                 letters: String): Iterator[Play] = {
    val lastIndex = ranges.length - 1
    for (startIndex <- 0 to lastIndex;
         endIndex <- startIndex to lastIndex;
         result <- rangesSlice(ranges, startIndex, endIndex)
           .options(rowIndex, row, letters))
      yield result
  }.iterator

  private def rangesSlice(ranges: List[Range], startIndex: Int, endIndex: Int) =
    RangesSlice(ranges, startIndex, endIndex, wordsService, wordsAcrossAreValid, createPlay)
}


