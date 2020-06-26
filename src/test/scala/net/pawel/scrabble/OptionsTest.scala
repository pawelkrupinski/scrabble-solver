package net.pawel.scrabble

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
}
