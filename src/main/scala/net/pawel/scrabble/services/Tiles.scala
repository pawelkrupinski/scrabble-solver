package net.pawel.scrabble.services

import net.pawel.scrabble.Tile

object Tiles {
  val score = Map(
    'a' -> 1,
    'b' -> 4,
    'c' -> 4,
    'd' -> 2,
    'e' -> 1,
    'f' -> 4,
    'g' -> 3,
    'h' -> 3,
    'i' -> 1,
    'j' -> 10,
    'k' -> 5,
    'l' -> 2,
    'm' -> 4,
    'n' -> 2,
    'o' -> 1,
    'p' -> 4,
    'q' -> 10,
    'r' -> 1,
    's' -> 1,
    't' -> 1,
    'u' -> 2,
    'v' -> 5,
    'w' -> 4,
    'x' -> 8,
    'y' -> 3,
    'z' -> 10,
  )

  def tile(char: Char) = Tile(char, score.get(char).get)
  def tiles(word: String) = word.map(tile).toList
}