package aoc2017

import utils.utils._

object day14 extends App {
  // https://adventofcode.com/2017/day/14

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test)
    val rowStrings: Seq[String] = "(\r*\n)".r.split(inp).toSeq

    rowStrings.foreach(println)

    (98, 98)
  }
  println(solveDay(day = 14, test = true))
  println(solveDay(day = 14, test = false))
}
