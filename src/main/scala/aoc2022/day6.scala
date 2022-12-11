package aoc2022

import utils.utils.readDay

object day6 extends App {
  // https://adventofcode.com/2022/day/6

  def testMarker(s: String): Int = {
    s.toCharArray.distinct.length
  }
  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test)

    def generalSolver(len: Int): Int = {
      inp.sliding(len).toList.              // sliding windows of the required length
        map(vindue => testMarker(vindue)).  // test uniqueness length of each window
        indexWhere(_ >= len) + len          // find the start of the marker plus the len to get the end
    }

    val part1 = generalSolver(4)
    val part2 = generalSolver(14)

    (part1, part2)
  }
  println(solveDay(6, true))
  println(solveDay(6))

}
