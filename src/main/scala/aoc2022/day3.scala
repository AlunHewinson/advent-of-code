package aoc2022

import utils.utils.readDay

object day3 extends App {
  // https://adventofcode.com/2022/day/3

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test)
    val backpacks: Array[Array[Char]] = "\r*\n".r.split(inp).map(_.toArray)
    val intersects = backpacks.map(x => {
      val xl: Int = x.length
      val elf1: Array[Char] = x.slice(0, xl / 2)
      val elf2: Array[Char] = x.slice(xl / 2, xl)
      val overlap: Int = (elf1 intersect elf2).map(_.toInt).min
      (overlap - 38) % 58 // evil hack!
    })
    val elfGroups = backpacks.sliding(3, 3).toArray
    val badges = elfGroups.map(elfGroup => {
      val badge = elfGroup.reduce((x, y) => x intersect y).min
      (badge - 38) % 58 // evil hack!
    })
    (intersects.sum, badges.sum)
  }
    println(solveDay(3, true))
    println(solveDay(3))
}
