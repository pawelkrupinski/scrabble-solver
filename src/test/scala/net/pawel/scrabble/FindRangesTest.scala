package net.pawel.scrabble

import net.pawel.scrabble.services.{FindRanges, Tiles}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar

class FindRangesTest extends AnyFlatSpec with Matchers with MockitoSugar {

  "CalculateCrossingWords" should "return a range for single tile" in {
    val ranges: List[services.Range] = FindRanges(List(tile()))

    ranges shouldBe List(range(0, 0, 1))
  }

  it should "return a range for single tile not at the beginning" in {
    val ranges: List[services.Range] = FindRanges(List(None, tile(), None))

    ranges shouldBe List(range(1, 1, 1))
  }

  it should "return a range for multiple tiles next to each other" in {
    val ranges: List[services.Range] = FindRanges(List(None, tile(), tile(), None, None))

    ranges shouldBe List(range(1, 2, 2))
  }

  it should "calculate complex range" in {
    val row = List(None, tile(), tile(), None, tile(), None, tile(), tile(), tile())
    val ranges: List[services.Range] = FindRanges(row)

    ranges shouldBe List(range(1, 2, 2), range(4, 4, 1), range(6, 8, 3))
  }

  def range(startIndex: Int, endIndex: Int, numberOfTiles: Int) =
    services.Range(startIndex, endIndex, List.fill(numberOfTiles)(anyTile()))

  def tile() = Some(anyTile)

  private def anyTile() = Tiles.tile('a')
}
