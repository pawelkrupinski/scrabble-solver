package net.pawel.scrabble

import net.pawel.scrabble.services.Words
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar

class OptionsTest extends AnyFlatSpec with Matchers with MockitoSugar {

  "Options" should "generate tada" in {
    val board = Board().add("begat", 3, 7)
    val game = Game(board)

    val options = game.options("tada").toList
    options should not be empty
    val tadas = options.filter(_.word.string() == "tada")
    tadas should not be empty
    val found = tadas.find(_.wordsAcross.map(_.string()).toSet == Set("ab", "de", "ag"))
    found should not be empty
  }

  it should "cover all the letters in all the ranges" in {
    val board = Board().add("e", 0, 3).add("e", 0, 5)
    val words = Words(List("gene"))
    val game = Game(board, words)

    val options = game.options("gn").toList
    options should not be empty
    options should have size 1

    val play = options.head
    val word = play.word
    word shouldBe WordPlayed(0, 2, Word("gene"), Across)

  }

  it should "cover all the letters in all the ranges 2" in {
    val board = Board().add("a", 0, 3).add("a", 0, 5)
    val words = Words(List("abad"))
    val game = Game(board, words)

    val options = game.options("bd").toList
    options should not be empty
    options should have size 1

    val play = options.head
    val word = play.word
    word shouldBe WordPlayed(0, 3, Word("abad"), Across)

  }
}
