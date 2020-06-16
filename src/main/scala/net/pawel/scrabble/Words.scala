package net.pawel.scrabble

import scala.collection.immutable.TreeSet
import scala.io.Source


object Words {
  private val Filename = "/words.txt"

  private lazy val words = loadWords()

  private def loadWords() = {
    val inputStream = classOf[Cell].getResourceAsStream(Filename)
    Source.fromInputStream(inputStream).getLines.map(_.toLowerCase).toList
  }

  def apply(words: List[String]) = new Words(words)

  def makeWords() = Words(words)
}

class Words(val words: List[String]) {
  val wordsSortedSet = TreeSet[String]() ++ words

  def isValid(word: String): Boolean = wordsSortedSet.contains(word)

  def matchRegex(regex: String) =
    words.filter(_.matches(regex))


  def wordsSpelledBy(letters: String) =
    words.filter(possibleMatch(letters)).filter(exactMatch(letters))

  private def possibleMatch(letters: String)(word: String) = 
    word.forall(letter => letters.contains(letter))

  private def exactMatch(letters: String)(word: String): Boolean =
    oneCanSpellTheWordUsingTheLetters(letters, word)

  private def oneCanSpellTheWordUsingTheLetters(letters: String, word: String) =
    letters.foldLeft(word.toList)((word, char) => removeFirstMatch(word, char)).isEmpty

  private def removeFirstMatch(string: List[Char], char: Char): List[Char] = string match {
    case Nil => Nil
    case head :: tail if head == char || char == ' ' => tail
    case head :: tail => head :: removeFirstMatch(tail, char)
  }
}
