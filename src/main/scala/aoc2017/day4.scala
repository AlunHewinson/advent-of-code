package aoc2017

import utils.utils._

object day4 extends App {
  // https://adventofcode.com/2017/day/4

  def alphabetise(word: String): String = {
    word.toCharArray.sorted.mkString("")
  }

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test, 2017)
    val rowStrings: Seq[String] = "(\r*\n)".r.split(inp).toSeq

    val phrases: Seq[Seq[String]] = rowStrings.map(phrase => phrase.split("[ \t]+").toSeq)

    val a1ValidPhrases: Seq[Seq[String]] = phrases.filter(words => words.length == words.toSet.count(_ => true))
    val a1 = a1ValidPhrases.length

    val a2ValidPhrases = phrases.filter(words => words.length == words.map(alphabetise).toSet.count(_ => true))
    val a2 = a2ValidPhrases.length

    (a1, a2)
  }
  println(solveDay(day = 4, test = true))
  println(solveDay(day = 4, test = false))
}
