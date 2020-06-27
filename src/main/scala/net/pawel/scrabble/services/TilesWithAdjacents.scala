package net.pawel.scrabble.services

import net.pawel.scrabble.Board

class TilesWithAdjacents(val board: Board) {

  private val tuples = (0 to 14).map(rowIndex => rowIndex -> calculate(rowIndex))
  private val values = Map.from(tuples)

  def indices(rowIndex: Int): List[Int] = values(rowIndex)

  def indicesWithinWord(rowIndex: Int, columnIndex: Int, word: String): List[Int] =
    indicesWithinWord(rowIndex, columnIndex, columnIndex + word.length - 1)

  private def indicesWithinWord(rowIndex: Int, startColumnIndex: Int, endColumnIndex: Int): List[Int] =
    indices(rowIndex).filter(isWithin(startColumnIndex, endColumnIndex))

  private def isWithin(startColumnIndex: Int, endColumnIndex: Int)(index: Int) =
    startColumnIndex <= index && index <= endColumnIndex

  private def calculate(rowIndex: Int): List[Int] = {
    val row = board.row(rowIndex)
    val previousRow = board.maybeRow(rowIndex - 1)
    val nextRow = board.maybeRow(rowIndex + 1)
    val indices = (0 to 14).filter(index =>
      row(index).isEmpty &&
        (previousRow.map(_(index).isDefined).getOrElse(false) ||
          nextRow.map(_(index).isDefined).getOrElse(false))
    )
    indices.toList
  }}
