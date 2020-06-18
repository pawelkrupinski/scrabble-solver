package net.pawel.scrabble

case class Range(startIndex: Int, endIndex: Int, tiles: List[Tile])

class CalculateCrossingWords(val game: Game,
                             val wordsService: Words,
                             val wordsAcross: WordsAcross,
                             val tilesWithAdjacents: TilesWithAdjacents) {
  val board = game.board

  def wordFits(word: List[Tile], row: List[Option[Tile]]): Boolean =
    word.zip(row).forall {
      case (wordTile, boardTile) => boardTile.isEmpty || wordTile == boardTile.get
    }

  def wordsAreValid(words: List[WordPlayed]): Boolean = words.map(_.string()).forall(wordsService.isValid)

  def wordsAcrossAreValid(wordPlayed: WordPlayed): IterableOnce[Play] = {
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

  def fitsInTheRow(rowIndex: Int,
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
      ).toList.flatMap(wordsAcrossAreValid)
  }

  def handleRangesFromTo(firstRangeIndex: Int,
                         lastRangeIndex: Int,
                         ranges: List[Range],
                         rowIndex: Int,
                         row: List[Option[Tile]],
                         letters: String): List[Play] = {
    val before = gapBefore(firstRangeIndex, ranges)
    val after = gapAfter(lastRangeIndex, ranges)
    val startIndex = ranges(firstRangeIndex).startIndex
    val endIndex = ranges(lastRangeIndex).endIndex + 1
    val tiles: List[Option[Tile]] = row.slice(startIndex, endIndex)
    val rangeLetters = tiles.flatMap(_.toList.map(_.letter)).mkString("")
    val regex = tiles
      .map {
        case Some(tile) => tile.letter
        case None => '.'
      }.mkString(s".{0,$before}", "", s".{0,$after}")
    val allLetters = rangeLetters + letters
    wordsService.wordsSpelledBy(allLetters)
      .filter(_.matches(regex))
      .filterNot(word => ranges.size == 1 && (word.length == endIndex - startIndex))
      .flatMap(fitsInTheRow(rowIndex, row, startIndex - before, endIndex + after, rangeLetters.length + 7))
  }

  private def gapBefore(firstRangeIndex: Int, ranges: List[Range]): Int = {
    val isFirst = firstRangeIndex == 0
    val startRange = ranges(firstRangeIndex)
    if (isFirst) {
      startRange.startIndex
    } else {
      val previousRange = ranges(firstRangeIndex - 1)
      startRange.startIndex - previousRange.endIndex - 2
    }
  }

  private def gapAfter(lastRangeIndex: Int, ranges: List[Range]): Int = {
    val isLast = lastRangeIndex == ranges.length - 1

    val endRange = ranges(lastRangeIndex)
    if (isLast) {
      14 - endRange.endIndex
    } else {
      val followingRange = ranges(lastRangeIndex + 1)
      followingRange.startIndex - endRange.endIndex - 2
    }
  }

  def handleRanges(ranges: List[Range], rowIndex: Int, row: List[Option[Tile]], letters: String): List[Play] = {
    val lastIndex = ranges.length - 1
    for (startIndex <- 0 to lastIndex;
         endIndex <- startIndex to lastIndex;
         result <- handleRangesFromTo(startIndex, endIndex, ranges, rowIndex, row, letters))
      yield result
  }.toList

  def calculateCrossingWords(letters: String, rowIndex: Int): List[Play] = {
    val row = board.row(rowIndex)
    val ranges = FindRanges(row)

    handleRanges(ranges, rowIndex, row, letters)
  }
}
