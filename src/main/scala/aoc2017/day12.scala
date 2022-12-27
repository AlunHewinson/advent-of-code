package aoc2017

import utils.utils._

object day12 extends App {
  // https://adventofcode.com/2017/day/12

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test, 2017)
    val rowStrings: Seq[String] = "(\r*\n)".r.split(inp).toSeq

    rowStrings.foreach(println)

    (98, 98)
  }
  println(solveDay(day = 12, test = true))
  println(solveDay(day = 12, test = false))
}
