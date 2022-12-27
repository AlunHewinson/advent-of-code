package aoc2017

import utils.utils._

object day9 extends App {
  // https://adventofcode.com/2017/day/9

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test)
    val rowStrings: Seq[String] = "(\r*\n)".r.split(inp).toSeq

    rowStrings.foreach(println)

    (98, 98)
  }
  println(solveDay(day = 9, test = true))
  println(solveDay(day = 9, test = false))
}
