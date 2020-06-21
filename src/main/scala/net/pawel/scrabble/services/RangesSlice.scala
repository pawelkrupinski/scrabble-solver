package net.pawel.scrabble.services

import net.pawel.scrabble._

case class RangesSlice(private val ranges: List[Range],
                       private val firstRangeIndex: Int,
                       private val lastRangeIndex: Int,
                       private val wordsService: Words,
                       private val wordsAcrossAreValid: WordsAcrossAreValid) {

  val isFirst = firstRangeIndex == 0
  val isLast = lastRangeIndex == ranges.length - 1

  val firstRange = ranges(firstRangeIndex)
  val lastRange = ranges(lastRangeIndex)

  def options(rowIndex: Int,
              row: List[Option[Tile]],
              letters: String): List[Play] = {
    val before = gapBefore()
    val after = gapAfter()
    val startIndex = firstRange.startIndex
    val endIndex = lastRange.endIndex + 1
    val tiles: List[Option[Tile]] = row.slice(startIndex, endIndex)
    val rangeLetters = tiles.flatMap(_.toList.map(_.letter)).mkString("")
    val regex = tiles
      .map(tile => tile.map(_.letter).getOrElse('.'))
      .mkString(s".{0,$before}", "", s".{0,$after}")
    val allLetters = rangeLetters + letters
    val onlyOneRange = firstRangeIndex == lastRangeIndex
    wordsService.wordsSpelledBy(allLetters)
      .filter(_.matches(regex))
      .filterNot(word => {
        val wordCoversEntireRangesSpan = word.length == endIndex - startIndex
        onlyOneRange && wordCoversEntireRangesSpan
      })
      .flatMap(fitsInTheRow(rowIndex, row, startIndex - before, endIndex + after, rangeLetters.length + 7))
  }


  private def gapBefore(): Int = {
    if (isFirst) {
      firstRange.startIndex
    } else {
      val previousRange = ranges(firstRangeIndex - 1)
      firstRange.startIndex - previousRange.endIndex - 2
    }
  }

  private def gapAfter(): Int = {
    if (isLast) {
      14 - lastRange.endIndex
    } else {
      val followingRange = ranges(lastRangeIndex + 1)
      followingRange.startIndex - lastRange.endIndex - 2
    }
  }

  private def fitsInTheRow(rowIndex: Int,
                           row: List[Option[Tile]],
                           columnStartIndex: Int,
                           columnEndIndex: Int,
                           lengthForBonus: Int)(word: String): List[Play] = {
    val tiles = Tiles.tiles(word)
    val wordLength = word.length
    val start = columnStartIndex
    val end = scala.math.min(14 - wordLength + 1, columnEndIndex - wordLength + 1)

    def wordTouchesAnExistingLetter(columnIndex: Int) =
      row.slice(columnIndex, columnIndex + wordLength).exists(_.isDefined)

    def wordHasAGapBeforeAndAfter(startIndex: Int, endIndex: Int) =
      (startIndex == 0 || row(startIndex - 1).isEmpty) &&
        (endIndex == 14 || row(endIndex + 1).isEmpty)

    (start to end)
      .flatMap(columnIndex =>
        if (wordHasAGapBeforeAndAfter(columnIndex, columnIndex + word.length - 1) &&
          wordTouchesAnExistingLetter(columnIndex) &&
          wordFits(tiles, row.drop(columnIndex))) {
          val allLettersUsed = tiles.length == lengthForBonus
          List(WordPlayed(rowIndex, columnIndex, Word(tiles), Across, allLettersUsed))
        } else Nil
      ).toList.flatMap(wordsAcrossAreValid.apply)
  }

  private def wordFits(word: List[Tile], row: List[Option[Tile]]): Boolean =
    word.zip(row).forall {
      case (wordTile, boardTile) => boardTile.isEmpty || wordTile == boardTile.get
    }
}
