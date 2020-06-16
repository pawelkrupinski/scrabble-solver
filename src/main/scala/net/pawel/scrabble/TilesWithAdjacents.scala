package net.pawel.scrabble

class TilesWithAdjacents(val board: Board) {

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

  def indicesWithinWord(columnIndex: Int, lastColumnIndex: Int, indicesThatHaveAdjacents: List[Int]) = {
    indicesThatHaveAdjacents.filter(i => columnIndex <= i && i <= lastColumnIndex)
  }
}
