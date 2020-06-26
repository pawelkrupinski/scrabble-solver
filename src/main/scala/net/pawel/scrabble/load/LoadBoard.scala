package net.pawel.scrabble.load

import java.io.{BufferedWriter, File, PrintWriter}
import java.nio.file.Paths

import net.pawel.scrabble.games.Games.GameSetup
import net.pawel.scrabble.services.Tiles
import net.pawel.scrabble.{Board, Tile}

import scala.io.Source

object LoadBoard {

  lazy val parseBoard = new ParseBoard()

  def apply(filename: String): Option[Board] = {
    val parseBoard = new ParseBoard()

    lines(filename).map(lines => parseBoard(lines.drop(1)))
  }

  def loadLetters(filename: String): Option[String] = lines(filename).map(_.head)

  private def lines(filename: String): Option[List[String]] = {
    val file = fileAt(filename)
    val lines = if (file.exists()) {
      Some(Source.fromFile(file).getLines().toList)
    } else None
    lines
  }

  def fileAt(filename: String) = Files.fileAt(s"./games/$filename")

  def fromString(string: String) = parseBoard(string.split("\n").toList.filterNot(_.isEmpty))

  def boardFiles() = Files.fileAt(s"./games").listFiles().toList
    .map(file => GameSetup(removeExtension(file.getName), file.getName))

  private def removeExtension(name: String) = name.substring(0, name.lastIndexOf('.'))
}


object SaveBoard {
  def apply(yourNewLetters: String, board: Board, filename: String): Unit = {
    val file = LoadBoard.fileAt(filename)
    val writer = new BufferedWriter(new PrintWriter(file))
    writer.write(yourNewLetters + "\n")
    writer.write(PrintBoard(board))
    writer.close()
  }
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

object Files {
  def fileAt(filename: String) = {
    val currentDir = new File(Paths.get("").toUri)
    new File(currentDir, filename)
  }
}