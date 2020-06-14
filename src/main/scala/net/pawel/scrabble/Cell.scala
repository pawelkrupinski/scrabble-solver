package net.pawel.scrabble

trait Cell {
  def score(wordPoints: Int, letterPoints: Int): Int
}

case object Normal extends Cell {
  override def score(wordPoints: Int, letterPoints: Int): Int = wordPoints
}

case object DoubleWord extends Cell {
  override def score(wordPoints: Int, letterPoints: Int): Int = wordPoints * 2
}

case object TripleWord extends Cell {
  override def score(wordPoints: Int, letterPoints: Int): Int = wordPoints * 3
}

case object DoubleLetter extends Cell {
  override def score(wordPoints: Int, letterPoints: Int): Int = wordPoints + letterPoints
}

case object TripleLetter extends Cell {
  override def score(wordPoints: Int, letterPoints: Int): Int = wordPoints + (letterPoints * 2)
}
