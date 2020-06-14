package net.pawel.scrabble

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar


class ScrabbleTest extends AnyFlatSpec with Matchers with MockitoSugar {

  "Inventory" should "Be created empty" in {
    0 shouldBe 0
  }
}
