package aoc2022

import utils.utils.readDay

object day1 extends App {
  // https://adventofcode.com/2022/day/1

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test)
    val elfSeparator: String = "(\r*\n){2,}"
    val itemSeparator: String = "\r*\n"
    val elves: Array[String] = elfSeparator.r.split(inp)
    val snacks: Array[Int] = elves.map(elf => {
      val stringItems: Array[String] = itemSeparator.r.split(elf)
      stringItems.map(_.toInt).sum
    })
    (snacks.max, snacks.sorted(Ordering.Int.reverse).take(3).sum)
  }
  println(solveDay(1, true))
  println(solveDay(1))
}
