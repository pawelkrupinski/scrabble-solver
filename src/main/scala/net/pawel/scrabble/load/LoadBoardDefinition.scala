package net.pawel.scrabble.load

import net.pawel.scrabble._

import scala.io.Source

object ParseBoardDefinition {
  def apply(lines: List[String]) = BoardDefinition(lines.map(toRow))
  def toRow(string: String) = string.split(",").flatMap(toCells).toList

  def toCells(string: String): List[Cell] = string match {
    case "dl" => List(DoubleLetter)
    case "tl" => List(TripleLetter)
    case "dw" => List(DoubleWord)
    case "tw" => List(TripleWord)
    case number => List.fill(number.toInt)(Normal)
  }
}

object LoadBoardDefinition {
  val FileName = "/board.txt"

  val parseBoardDefinition = ParseBoardDefinition

  def apply(from: String): BoardDefinition = {
    val inputStream = classOf[Cell].getResourceAsStream(from)
    val lines = Source.fromInputStream(inputStream).getLines.toList
    parseBoardDefinition(lines)
  }

  def apply(): BoardDefinition = apply(FileName)
}
