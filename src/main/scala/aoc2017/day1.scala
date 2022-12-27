package aoc2017

import utils.utils._
import scala.annotation.tailrec

object day1 extends App {
  // https://adventofcode.com/2017/day/1

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test, 2017)
    val rowStrings: Seq[String] = "(\r*\n)".r.split(inp).toSeq

    rowStrings.foreach(println)

    (98, 98)
  }
  println(solveDay(day = 1, test = true))
  println(solveDay(day = 1, test = false))
}
