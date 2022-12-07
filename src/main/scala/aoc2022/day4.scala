package aoc2022

import utils.utils.readDay

object day4 extends App {
  // https://adventofcode.com/2022/day/4

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test)
    val pairs: Array[String] = "\r*\n".r.split(inp) // element like "2-4,6-8"
    val individuals: Array[Array[String]] = pairs.map(",".r.split(_)) // element like "2-4"
    val stringRanges: Array[Array[Array[String]]] = individuals.map(_.map("-".r.split(_)))
    val intRanges: Array[Array[Array[Int]]] = stringRanges.map(_.map(_.map(_.toInt)))

    //intRanges.map(_.map(_.map(println)))
    def fullyContains(x: Array[Int], y: Array[Int]): Boolean = {
      (x(0) <= y(0) & x(1) >= y(1)) | (x(0) >= y(0) & x(1) <= y(1))
    }
    def overlaps(x: Array[Int], y: Array[Int]): Boolean = {
      x(0) <= y(1) & y(0) <= x(1)
    }
    val countFullyContains: Int = intRanges.map(z => fullyContains(z(0), z(1))).count(q => q)
    val countOverlaps     : Int = intRanges.map(z => overlaps     (z(0), z(1))).count(q => q)
    (countFullyContains, countOverlaps)
  }
  println(solveDay(4, true))
  println(solveDay(4))
}
