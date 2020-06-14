package net.pawel.scrabble

import scala.collection.immutable.TreeSet
import scala.io.Source

object Words {
  private val Filename = "/words.txt"

  val words = loadWords()
  val wordsSortedSet = TreeSet[String]() ++ words

  def isValid(word: String): Boolean = wordsSortedSet.contains(word)

  def matchRegex(regex: String) = {
    words.filter(_.matches(regex))
  }

  def loadWords() = {
    val inputStream = classOf[Cell].getResourceAsStream(Filename)
    Source.fromInputStream(inputStream).getLines.map(_.toLowerCase).toList
  }
}
