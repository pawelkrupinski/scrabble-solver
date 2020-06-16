package net.pawel.scrabble

trait Cell {
  def letterScore(wordPoints: Int, letterPoints: Int) = wordPoints
  def wordScore(wordPoints: Int): Int = wordPoints
}

case object Normal extends Cell

case object DoubleWord extends Cell {
  override def wordScore(wordPoints: Int): Int = wordPoints * 2
}

case object TripleWord extends Cell {
  override def wordScore(wordPoints: Int): Int = wordPoints * 3
}

case object DoubleLetter extends Cell {
  override def letterScore(wordPoints: Int, letterPoints: Int): Int = wordPoints + letterPoints
}

case object TripleLetter extends Cell {
  override def letterScore(wordPoints: Int, letterPoints: Int): Int = wordPoints + (letterPoints * 2)
}
