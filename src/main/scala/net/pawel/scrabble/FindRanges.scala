package net.pawel.scrabble

object FindRanges {

  def apply(row: List[Option[Tile]]): List[Range] = {
    val padding = (None, -1)
    val augmentedRow = padding :: row.zipWithIndex ::: padding :: Nil
    val value = augmentedRow.sliding(2)
    val rangeIndices = value.flatMap(item => {
      val List((firstTile, firstIndex), (secondTile, secondIndex)) = item
      val startOfRange = firstTile.isEmpty && secondTile.isDefined
      val endOfRange = firstTile.isDefined && secondTile.isEmpty

      if (startOfRange)
        List(secondIndex)
      else if (endOfRange)
        List(firstIndex)
      else Nil
    })
    rangeIndices.sliding(2, step = 2).map(_.toList).map {
      case List(startIndex, endIndex) =>
        Range(startIndex, endIndex, row.slice(startIndex, endIndex + 1).map(_.get))
    }.toList
  }
}
