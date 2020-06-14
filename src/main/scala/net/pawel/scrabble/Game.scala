package net.pawel.scrabble

import net.pawel.scrabble.Main.runMain

object Danielle {
  def main(args: Array[String]): Unit = {
    val letters = "aasreeb"
    val filename = "games/Danielle.txt"

    Main.runMain(letters, filename)
  }
}

object Lynne {
  def main(args: Array[String]): Unit = {
    val letters = "anadoee"
    val filename = "games/Lynne.txt"

    runMain(letters, filename)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val letters = "anadoee"
    val filename = "current.txt"

    runMain(letters, filename)
  }

  def runMain(letters: String, filename: String): Unit = {
    val definition: BoardDefinition = LoadBoardDefinition()
    val board = LoadBoard(filename).getOrElse(Board())
    val game = Game(definition, board)
    mainLoop(game, letters)
  }

  def printOptions(options: List[ScoredPlay]) {
    options.take(40).foreach(printOption)
  }

  def printOption(scoredPlay: ScoredPlay) {
    val score = scoredPlay.score
    val play = scoredPlay.play
    val word = play.word
    val row = word.row
    val column = play.word.column
    val string = play.word.string()
    val direction = word.direction
    val words = play.wordsAcross.map(_.string()).mkString(", ")
    println(s"$score ($row, $column, $direction) $string : $words")
  }

  def mainLoop(game: Game, startLetters: String = "") = {
    var letters = startLetters

    if (game.board.isNotEmpty()) {
      printGame(game)
    }

    while (true) {
      if (letters.isEmpty) {
        print("Enter letters: ")
        letters = readLine()
      }
      val options = game.sortedOptions(letters)
      printOptions(options)

      print("Enter command: ")
      readLine() match {
        case "" => println("S")
        case _ => {}
      }
      printGame(game)
    }
  }

  private def printGame(game: Game) = {
    println(PrintBoard(game.board))
  }

  private def readLine() = Console.in.readLine()
}

case class Game(definition: BoardDefinition,
                board: Board = Board()) {

  class CalculateAdjacentWords {
    private def doesNotExceedLengthOfTheRow(lastIndex: Int) = lastIndex < 15

    def coversAnIndexThatHasAdjacents(index: Int, lastIndex: Int, indicesThatHaveAdjacents: List[Int]) =
      indicesThatHaveAdjacents.exists(i => index <= i && i <= lastIndex)

    def doesNotCoverPlacedTiles(index: Int, lastIndex: Int, row: List[Option[Tile]]): Boolean =
      !(index to lastIndex).exists(row(_).isDefined)

    def doesNotTouchPlacedTilesInRow(index: Int, lastIndex: Int, row: List[Option[Tile]]): Boolean =
      (index == 0 || row(index - 1).isEmpty) && (lastIndex == 14 || row(lastIndex + 1).isEmpty)

    def formsValidWords(word: String,
                        rowIndex: Int,
                        columnIndex: Int,
                        lastColumnIndex: Int,
                        indicesThatHaveAdjacents: List[Int]): Boolean = {
      val indices = indicesWithinWord(columnIndex, lastColumnIndex, indicesThatHaveAdjacents)

      acrossWordsAreValid(word, rowIndex, columnIndex, indices)
    }

    private def wordsAcross(word: String,
                            rowIndex: Int,
                            columnIndex: Int,
                            lastColumnIndex: Int,
                            indicesThatHaveAdjacents: List[Int]) = {
      val indices = indicesWithinWord(columnIndex, lastColumnIndex, indicesThatHaveAdjacents)

      def letterAt(i: Int) = word(i - columnIndex)

      val words = indices.map(columnIndex => acrossWord(rowIndex, columnIndex, letterAt(columnIndex)))
      words
    }

    def tryWord(word: String,
                rowIndex: Int,
                columnIndex: Int,
                row: List[Option[Tile]],
                indicesThatHaveAdjacents: List[Int]): Option[Play] = {
      val lastColumnIndex = columnIndex + word.length - 1
      if (doesNotExceedLengthOfTheRow(lastColumnIndex) &&
        coversAnIndexThatHasAdjacents(columnIndex, lastColumnIndex, indicesThatHaveAdjacents) &&
        doesNotCoverPlacedTiles(columnIndex, lastColumnIndex, row) &&
        doesNotTouchPlacedTilesInRow(columnIndex, lastColumnIndex, row) &&
        formsValidWords(word, rowIndex, columnIndex, lastColumnIndex, indicesThatHaveAdjacents)) {
        val wordPlayed = WordPlayed(rowIndex, columnIndex, Word(word))
        val words = wordsAcross(word, rowIndex, columnIndex, lastColumnIndex, indicesThatHaveAdjacents)
        val game = update(word, rowIndex, columnIndex)
        Some(Play(game, wordPlayed, words))
      } else None
    }

    def adjacentWordOptions(words: List[String], rowIndex: Int) = {
      val row = board.row(rowIndex)
      val indices = indicesOfCellsThatHaveAdjacents(rowIndex)
      if (indices.isEmpty) Nil
      else {
        val firstIndex = indices.head
        val lastIndex = indices.last
        for (word <- words;
             length = word.length;
             start = scala.math.max(firstIndex - length + 1, 0);
             end = scala.math.min(lastIndex, 14 - length + 1);
             index <- (start to end).toList;
             result <- tryWord(word, rowIndex, index, row, indices).toList)
          yield result
      }
    }
  }

  //    def options(letters: String) = {
  //    val url = s"https://word.tips/words-for/$letters/?dictionary=wwf"
  //    val value = Unirest.get(url).asString().getBody
  //
  //    val words = Jsoup.parse(value).select("a.word-link").asScala.toList.map(_.text())
  //    words
  //  }

  private def wordsSpelledBy(letters: String) = {
    Words.words.filter(possibleMatch(letters)).filter(exactMatch(letters))
  }

  private def exactMatch(letters: String)(word: String): Boolean =
    oneCanSpellTheWordUsingTheLetters(letters, word)

  private def oneCanSpellTheWordUsingTheLetters(letters: String, word: String) = {
    letters.foldLeft(word.toList)((word, char) => removeFirstMatch(word, char)).isEmpty
  }

  def transposed(): Game = copy(board = board.transposed(), definition = definition.transposed())

  def sortedOptions(letters: String) =
    options(letters).map(_.score(definition, board)).sortBy(_.score).reverse

  def options(letters: String): List[Play] = calculateOptions(letters) ::: transposedOptions(letters)

  private def transposedOptions(letters: String) = transposed().calculateOptions(letters).map(_.transposed())

  case class Range(startIndex: Int, endIndex: Int, tiles: List[Tile])

  def groupConsecutive(row: List[Option[Tile]],
                       index: Int = 0,
                       startIndex: Int = -1,
                       rangeTiles: List[Tile] = Nil,
                       result: List[Range] = Nil): List[Range] = {
    val inRange = startIndex != -1

    def range() = Range(startIndex, index - 1, rangeTiles)

    row match {
      case Nil if inRange => result ++ List(range)
      case Nil => result
      case Some(head) :: tail if inRange => groupConsecutive(tail, index + 1, startIndex, rangeTiles ::: List(head), result)
      case Some(head) :: tail if !inRange => groupConsecutive(tail, index + 1, index, List(head), result)
      case None :: tail if inRange => groupConsecutive(tail, index + 1, -1, Nil, result ++ List(range()))
      case None :: tail if !inRange => groupConsecutive(tail, index + 1, -1, rangeTiles, result)
    }
  }

  def wordFits(word: List[Tile], row: List[Option[Tile]]): Boolean =
    word.zip(row).forall {
      case (wordTile, boardTile) => boardTile.isEmpty || wordTile == boardTile.get
    }

  def wordsAreValid(words: List[WordPlayed]): Boolean = words.map(_.string()).forall(Words.isValid)

  def wordsAcrossAreValid(wordPlayed: WordPlayed): IterableOnce[Play] = {
    val rowIndex = wordPlayed.row
    val indices = tilesThatHaveAdjacentsWithinWord(rowIndex, wordPlayed)
    val columnIndex = wordPlayed.column
    val word = wordPlayed.string()
    val wordsAcross = acrossWords(word, rowIndex, columnIndex, indices)
    if (wordsAreValid(wordsAcross)) {
      val play = Play(update(word, rowIndex, columnIndex), wordPlayed, wordsAcross)
      List(play)
    } else Nil
  }

  private def acrossWordsAreValid(word: String, rowIndex: Int, columnIndex: Int, indicesWithinWord: List[Int]) = {
    val words = acrossWords(word, rowIndex, columnIndex, indicesWithinWord)
    words.map(_.string()).forall(Words.isValid)
  }

  private def acrossWords(word: String, rowIndex: Int, columnIndex: Int, indicesWithinWord: List[Int]) = {
    def letterAt(i: Int) = word(i - columnIndex)

    indicesWithinWord.map(index => acrossWord(rowIndex, index, letterAt(index)))
  }

  def acrossWordIsValid(rowIndex: Int, columnIndex: Int, char: Char): Boolean = {
    val word: WordPlayed = acrossWord(rowIndex, columnIndex, char)
    Words.isValid(word.string())
  }

  private def acrossWord(rowIndex: Int, columnIndex: Int, char: Char) = {
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

  def fitsInTheRow(rowIndex: Int, row: List[Option[Tile]])(word: String): List[Play] = {
    val tiles = Tiles.tiles(word)
    val wordLength = word.length
    val start = scala.math.max(row.indexWhere(_.isDefined) - wordLength + 1, 0)
    val end = scala.math.max(14 - wordLength + 1, 0)

    def wordTouchesAnExistingLetter(columnIndex: Int) =
      row.slice(columnIndex, columnIndex + wordLength).exists(_.isDefined)

    (start to end)
      .flatMap(columnIndex =>
        if (wordTouchesAnExistingLetter(columnIndex) &&
          wordFits(tiles, row.drop(columnIndex))) {
          List(WordPlayed(rowIndex, columnIndex, Word(tiles), Across))
        } else Nil
      ).toList.flatMap(wordsAcrossAreValid)
  }

  def calculateCrossingWords(letters: String, rowIndex: Int): List[Play] = {
    val row = board.row(rowIndex)
    val ranges = groupConsecutive(row)
    if (!ranges.isEmpty) {
      handleFirstRange(letters, rowIndex, ranges.head) ++
        handleLastRange(letters, rowIndex, ranges.last)
    } else Nil
  }

  private def handleFirstRange(letters: String, rowIndex: Int, firstRange: Range): List[Play] = {
    val row = board.row(rowIndex)
    val firstIndex = firstRange.startIndex
    if (firstIndex > 0) {
      val rangeLetters = firstRange.tiles.map(_.letter).mkString("")
      val regex = s".{1,$firstIndex}" + rangeLetters
      wordsSpelledBy(rangeLetters + letters)
        .filter(_.matches(regex))
        .flatMap(fitsInTheRow(rowIndex, row))
    } else Nil
  }

  private def handleLastRange(letters: String, rowIndex: Int, lastRange: Range): List[Play] = {
    val row = board.row(rowIndex)
    val endIndex = lastRange.endIndex
    if (endIndex < 14) {
      val rangeLetters = lastRange.tiles.map(_.letter).mkString("")
      val length = 14 - endIndex
      val regex = rangeLetters + s".{1,$length}"
      val words = wordsSpelledBy(rangeLetters + letters)
        .filter(_.matches(regex))
        .flatMap(fitsInTheRow(rowIndex, row))
      words
    } else Nil
  }

  def calculateOptions(letters: String): List[Play] = {
    val calculateAdjacentWords = new CalculateAdjacentWords()
    val words = wordsSpelledBy(letters)
    (0 to 14).flatMap(rowIndex => {
      calculateAdjacentWords.adjacentWordOptions(words, rowIndex) ++
        calculateCrossingWords(letters, rowIndex)
    }).toList
  }

  def tilesThatHaveAdjacentsWithinWord(rowIndex: Int, wordPlayed: WordPlayed): List[Int] =
    tilesThatHaveAdjacentsWithinWord(rowIndex, wordPlayed.column, wordPlayed.lastIndex)

  def tilesThatHaveAdjacentsWithinWord(rowIndex: Int, startColumnIndex: Int, endColumnIndex: Int): List[Int] =
    indicesWithinWord(startColumnIndex, endColumnIndex, indicesOfCellsThatHaveAdjacents(rowIndex))

  def indicesOfCellsThatHaveAdjacents(rowIndex: Int): List[Int] = {
    val row = board.row(rowIndex)
    val previousRow = board.maybeRow(rowIndex - 1)
    val nextRow = board.maybeRow(rowIndex + 1)
    val indices = (0 to 14).filter(index =>
      row(index).isEmpty &&
        (previousRow.map(_ (index).isDefined).getOrElse(false) ||
          nextRow.map(_ (index).isDefined).getOrElse(false))
    )
    indices.toList
  }

  private def indicesWithinWord(columnIndex: Int, lastColumnIndex: Int, indicesThatHaveAdjacents: List[Int]) = {
    indicesThatHaveAdjacents.filter(i => columnIndex <= i && i <= lastColumnIndex)
  }


  private def removeFirstMatch(string: List[Char], char: Char): List[Char] = string match {
    case Nil => Nil
    case head :: tail if head == char || char == ' ' => tail
    case head :: tail => head :: removeFirstMatch(tail, char)
  }

  private def possibleMatch(letters: String)(word: String) = {
    word.forall(letter => letters.contains(letter))
  }

  def update(word: String, rowIndex: Int, columnIndex: Int): Game = {
    val tiles = Tiles.tiles(word)
    copy(board = board.add(tiles, rowIndex, columnIndex))
  }
}
