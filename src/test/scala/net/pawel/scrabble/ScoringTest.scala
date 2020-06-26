package net.pawel.scrabble

import net.pawel.scrabble.load.LoadBoardDefinition
import net.pawel.scrabble.services.Words
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar

class ScoringTest extends AnyFlatSpec with Matchers with MockitoSugar {

  val boardDefinition = LoadBoardDefinition()

  "Scoring" should "apply bonuses to crossing words for all the newly placed letters" in {
    val words = Words(List(
      "zoea",
      "za",
      "et"
    ))

    val board = Board()
      .addDown("smarts", 4, 5)
      .add("swords", 7, 2)
      .add("rebased", 9, 1)

    val game = Game(wordsService = words, board = board)
    val options = game.options("ze")
    val option = options.find(_.word.string() == "zoea").get

    option.score(boardDefinition, board).score shouldBe 48
  }
}
