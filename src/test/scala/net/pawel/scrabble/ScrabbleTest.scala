package net.pawel.scrabble

import net.pawel.scrabble.load.{LoadBoard, LoadBoardDefinition}
import net.pawel.scrabble.services.Words
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar


class ScrabbleTest extends AnyFlatSpec with Matchers with MockitoSugar {

  val boardDefinition = LoadBoardDefinition()

  "Ranges options" should "be generated" in {
    val letters = "efghijklmnop"

    val words = Words(List(
      "efa",
      "efagb",
      "aebfg",
      "aebfghc",
      "aebfghci",
      "def",
      "efdg",
      "efdgh"
    ))

    val boardString =
    """
      |_______________
      |_______________
      |___a___________
      |_______________
      |___b___________
      |_______________
      |_______________
      |_______________
      |___c___________
      |_______________
      |_______________
      |_______________
      |___d___________
      |_______________
      |_______________
      |""".stripMargin

    val board = LoadBoard.fromString(boardString)
    val game = Game(board = board, wordsService = words)
    val options = game.options(letters).toList
    val optionWords = options.map(_.word.string())
    options.length shouldBe 12
    optionWords.toSet shouldBe Set(
      "efa",
      "efagb",
      "aebfg",
      "aebfghc",
      "aebfghci",
      "def",
      "efdg",
      "efdgh")
  }

  "Scoring" should "should apply letter multiplication before word multiplication" in {
    val words = Words(List(
      "rebase",
      "smarts"
    ))

    val boardString =
      """
        |_______________
        |_______________
        |_______________
        |_______________
        |____ls_________
        |_____m_________
        |____la_________
        |____lrl________
        |_____t_________
        |_______________
        |_______________
        |_______________
        |_______________
        |_______________
        |_______________
        |""".stripMargin

    val board = LoadBoard.fromString(boardString)
    val game = Game(board = board, wordsService = words)
    val options = game.options("rebase").filter(play => play.word.string() == "rebase").toList
    options.length shouldBe 1
    options.head.score(boardDefinition, board).score shouldBe 33
  }

  "Scoring" should "not apply letter multipliers for across words" in {
    val letters = "leeks"

    val words = Words(List(
      "leeks",
      "jarls"
    ))

    val boardString =
      """
        |_______________
        |_______________
        |_______________
        |_______________
        |_______________
        |_jarl__________
        |_______________
        |_______________
        |_______________
        |_______________
        |_______________
        |_______________
        |_______________
        |_______________
        |_______________
        |""".stripMargin

    val board = LoadBoard.fromString(boardString)
    val game = Game(board = board, wordsService = words)
    val options = game.options(letters).filter(play => play.word.string() == "leeks" &&
      play.word.row == 1).toList
    options.length shouldBe 1
    options.toList.head.score(boardDefinition, board).score shouldBe 41
  }
}
