package net.pawel.scrabble

import net.pawel.scrabble.services.{Tiles, TilesWithAdjacents}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar

class TilesWithAdjacentsTest extends AnyFlatSpec with Matchers with MockitoSugar {

  "TilesWithAdjacents" should "work" in {
    var board = repeat(Board(), 0, List(0, 1))
    board = repeat(board, 2, List(1, 0))
    board = add(board, 5, List(1, 1, 1, 1, 1))
    board = add(board, 6, List(1, 1))

    val tilesWithAdjacents = new TilesWithAdjacents(board)
    tilesWithAdjacents.indices(1) shouldBe (0 to 14)
    tilesWithAdjacents.indices(3) shouldBe indicesOfOnes(repeat(List(1, 0))).toList
    tilesWithAdjacents.indices(4) shouldBe (0 to 4)
    tilesWithAdjacents.indices(6) shouldBe (2 to 4)
    tilesWithAdjacents.indices(8) shouldBe Nil
  }

  "TilesWithAdjacents" should "handle first row" in {
    val board = repeat(Board(), 1, List(0, 1))

    val tilesWithAdjacents = new TilesWithAdjacents(board)

    tilesWithAdjacents.indices(0) shouldBe indicesOfOnes(repeat(List(0, 1))).toList
  }

  "TilesWithAdjacents" should "handle last row" in {
    val board = repeat(Board(), 13, List(0, 1))

    val tilesWithAdjacents = new TilesWithAdjacents(board)

    tilesWithAdjacents.indices(14) shouldBe indicesOfOnes(repeat(List(0, 1))).toList
  }

  "TilesWithAdjacents" should "fetches indices within word" in {
    {
      val board = repeat(Board(), 1, List(1, 0))

      val tilesWithAdjacents = new TilesWithAdjacents(board)

      tilesWithAdjacents.indicesWithinWord(0, 2, "aaa") shouldBe List(2, 4)
    }
    {
      val board = repeat(Board(), 1, List(1))

      val tilesWithAdjacents = new TilesWithAdjacents(board)

      tilesWithAdjacents.indicesWithinWord(0, 2, "aaa") shouldBe List(2, 3, 4)
    }
  }

  def repeat(board: Board, rowIndex: Int, tiles: List[Int]): Board = {
    val row = repeat(tiles)
    val indices = indicesOfOnes(row)

    add(board, rowIndex, indices)
  }


  private def add(board: Board, rowIndex: Int, onesAndZeros: List[Int]): Board =
    add(board, rowIndex, indicesOfOnes(onesAndZeros.iterator))


  private def add(board: Board, rowIndex: Int, indices: Iterator[Int]): Board = {
    val tile = List(Tiles.tile('a'))
    indices.foldLeft(board)((board, columnIndex) => board.add(tile, rowIndex, columnIndex))
  }

  private def indicesOfOnes(row: Iterator[Int]) =
    row.zipWithIndex.flatMap {
      case (0, _) => Nil
      case (1, index) => List(index)
    }

  private def repeat(tiles: List[Int]) =
    Iterator.continually(tiles).flatten.take(15)
}
