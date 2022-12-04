package aoc2022

import utils.utils.readDay

object day2 extends App {
  // https://adventofcode.com/2022/day/2

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test)
    val games = "\r*\n".r.split(inp)
    val scoreNaive = Map(
      "A X" -> 4, "A Y" -> 8, "A Z" -> 3,
      "B X" -> 1, "B Y" -> 5, "B Z" -> 9,
      "C X" -> 7, "C Y" -> 2, "C Z" -> 6
    )
    val scoreBetter = Map(
      "A X" -> 3, "A Y" -> 4, "A Z" -> 8,
      "B X" -> 1, "B Y" -> 5, "B Z" -> 9,
      "C X" -> 2, "C Y" -> 6, "C Z" -> 7
    )
    (games.map(scoreNaive).sum, games.map(scoreBetter).sum)
    //(98, 98)
  }
  println(solveDay(2, true))
  println(solveDay(2))

}
