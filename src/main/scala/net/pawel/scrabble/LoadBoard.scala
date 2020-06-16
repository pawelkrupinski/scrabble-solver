package net.pawel.scrabble

import java.io.{File, StringBufferInputStream, StringReader}
import java.nio.file.{Path, Paths}

import scala.io.Source

object LoadBoard {
  val Filename = "games/current.txt"

  lazy val parseBoard = new ParseBoard()

  def apply(): Option[Board] = apply(Filename)

  def apply(filename: String): Option[Board] = {
    val parseBoard = new ParseBoard()

    val currentDir = new File(Paths.get("").toUri)

    val file = new File(currentDir, filename)
    if (file.exists()) {
      val lines = Source.fromFile(file).getLines().toList
      Some(parseBoard(lines))
    } else None
  }

  def fromString(string: String) = parseBoard(string.split("\n").toList.filterNot(_.isEmpty))
}

class ParseBoard() {
  def apply(lines: List[String]): Board = {
    Board(lines.map(_.map(toTile).toList))
  }

  private def toTile(char: Char): Option[Tile] =
    char match {
      case '_' => None
      case _ => Some(Tiles.tile(char))
    }
}

object PrintBoard {
  def apply(board: Board) =
    board.tiles.map(_.map(printTile).mkString).mkString("\n")

  private def printTile(tile: Option[Tile]) = tile.map(_.letter).getOrElse('_')
}