package net.pawel.scrabble

import net.pawel.scrabble.services.Tiles

case class BoardDefinition(private val cells: List[List[Cell]]) {
  def transposed(): BoardDefinition = copy(cells = cellsTransposed)

  def cellAt(row: Int, column: Int): Cell = cells(row)(column)

  private lazy val cellsTransposed = cells.transpose

  def row(row: Int) = cells(row)
  def column(column: Int) = cellsTransposed(column)
}

case class Tile(letter: Char, score: Int)

case class Board(tiles: List[List[Option[Tile]]] = List.fill(15)(List.fill(15)(None))) {
  def transposed(): Board = copy(tiles = tiles.transpose)

  def isEmpty() = tiles.forall(_.forall(_.isEmpty))
  def isNotEmpty() = !isEmpty()

  private val transposedTiles = tiles.transpose

  def row(row: Int) = tiles(row)
  def column(column: Int) = transposedTiles(column)

  def maybeRow(index: Int): Option[List[Option[Tile]]] =
    if (index < 0 || index > 14) None else Some(row(index))

  def maybeColumn(index: Int): Option[List[Option[Tile]]] =
    if (index < 0 || index > 14) None else Some(column(index))

  def add(word: String, rowIndex: Int, columnIndex: Int): Board =
    add(Tiles.tiles(word), rowIndex, columnIndex)

  def addDown(word: String, rowIndex: Int, columnIndex: Int): Board =
    transposed().add(Tiles.tiles(word), columnIndex, rowIndex).transposed()

  def add(word: List[Tile], rowIndex: Int, columnIndex: Int): Board = {
    copy(tiles = tiles.updated(rowIndex, updateRow(row(rowIndex), columnIndex, word)))
  }

  def updateRow(row: List[Option[Tile]], columnIndex: Int, word: List[Tile]): List[Option[Tile]] = word match {
    case Nil => row
    case head :: tail => updateRow(row.updated(columnIndex, Some(head)), columnIndex + 1, tail)
  }
}

case class PositionedTile(tile: Tile, cell: Cell)

object Word {
  def apply(word: String): Word = Word(Tiles.tiles(word))
}

case class Word(letters: List[Tile]) {
  def score(cells: List[Cell]): Int = {
    val startScore = basicScore()
    val currentScore = letters.zip(cells).foldLeft(startScore)((currentScore, tuple) => {
      val (tile, cell) = tuple
      cell.letterScore(currentScore, tile.score)
    })
    val result = cells.foldLeft(currentScore)((currentScore, cell) => cell.wordScore(currentScore))
    result
  }

  def basicScore(): Int = letters.map(_.score).sum

  def length() = letters.length

  def +(char: Char): Word = copy(letters = letters ::: List(Tiles.tile(char)))
  def +(string: String): Word = string.foldLeft(this)((word, char) => word + char)
  def word() = letters.map(_.letter).mkString("")
}

trait Direction {
  def cellsMatching(boardDefinition: BoardDefinition,
                    previousBoard: Board,
                    row: Int,
                    column: Int,
                    length: Int): List[Cell]

  def flip: Direction
}

case object Down extends Direction {
  override def flip: Direction = Across

  override def cellsMatching(boardDefinition: BoardDefinition,
                             previousBoard: Board,
                             row: Int, column: Int, length: Int): List[Cell] = {
    boardDefinition.column(column).slice(row, row + length)
      .zip(previousBoard.column(column).slice(row, row + length))
      .map {
        case (definition, previous) => if (previous.isDefined) Normal else definition
      }
  }
}

case object Across extends Direction {
  override def flip: Direction = Down

  override def cellsMatching(boardDefinition: BoardDefinition,
                             previousBoard: Board,
                             row: Int, column: Int, length: Int): List[Cell] = {
    boardDefinition.row(row).slice(column, column + length)
      .zip(previousBoard.row(row).slice(column, column + length))
      .map {
        case (definition, previous) => if (previous.isDefined) Normal else definition
      }
  }

}

case class WordPlayed(row: Int, column: Int, word: Word, direction: Direction = Across,
                      allLettersUsed: Boolean = false) {
  def score(boardDefinition: BoardDefinition,
            previousBoard: Board): Int = {
    val cells = direction
       .cellsMatching(boardDefinition, previousBoard, row, column, word.length())
    val allLettersUsedBonus = if (allLettersUsed) 35 else 0
    word.score(cells) + allLettersUsedBonus
  }

  def scoreNoBonuses(): Int = word.basicScore()

  def lastIndex: Int = {
    val startIndex = direction match {
      case Across => column
      case Down => row
    }
    startIndex + word.length() - 1
  }

  def transposed() = copy(row = column, column = row, direction = direction.flip)
  def +(char: Char): WordPlayed = copy(word = word + char)
  def +(string: String): WordPlayed = copy(word = word + string)
  def string(): String = word.word()
}

case class Play(game: Game, word: WordPlayed, wordsAcross: List[WordPlayed]) {
  def transposed() = copy(
    game = game.transposed(),
    word = word.transposed(),
    wordsAcross = wordsAcross.map(_.transposed())
  )

  def score(boardDefinition: BoardDefinition, previousBoard: Board): ScoredPlay = {
    val words = word :: wordsAcross
    val score = words.map(_.score(boardDefinition, previousBoard)).sum
    ScoredPlay(this, score)
  }
}

case class ScoredPlay(play: Play, score: Int)
