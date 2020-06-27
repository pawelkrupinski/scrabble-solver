package net.pawel.scrabble.services

import java.io.{BufferedWriter, PrintWriter}

import net.pawel.scrabble.WordPlayed
import net.pawel.scrabble.load.Files

import scala.collection.immutable.TreeSet
import scala.io.Source


object Words {
  private val Filename = "words.txt"

  private val WordsFile = Files.fileAt(s"src/main/resources/$Filename")

  private lazy val words = loadWords()

  private def loadWords() =
    Source.fromFile(WordsFile).getLines.map(_.toLowerCase).toList

  def apply(words: List[String]) = new Words(words)

  def makeWords() = Words(words)

  def remove(word: String) = {
    val writer = new BufferedWriter(new PrintWriter(WordsFile))
    words.filterNot(_ == word)
      .map(_.toUpperCase)
      .foreach(word => {
        writer.write(word + "\n")
      })
    writer.close()
  }
}

class Words(private val words: List[String]) {
  private val wordsSortedSet = TreeSet[String]() ++ words

  def isValid(word: String): Boolean = wordsSortedSet.contains(word)

  def isValid(word: WordPlayed): Boolean = isValid(word.string())

  def wordsSpelledBy(letters: String) =
    iteratorWordsSpelledBy(letters).toList

  def iteratorWordsSpelledBy(letters: String) =
    words.iterator.filter(possibleMatch(letters)).filter(exactMatch(letters))

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
