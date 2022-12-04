package aoc2022

import scala.io.Source
import scala.util.Using

object day4 extends App {
  // https://adventofcode.com/2022/day/4

  val day04: String = Using(Source.fromFile("input/2022/day04.txt")) { source => source.mkString }.get

}
