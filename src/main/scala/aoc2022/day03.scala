package aoc2022

import scala.io.Source
import scala.util.Using

object day03 extends App {
  // https://adventofcode.com/2022/day/3

  val day03: String = Using(Source.fromFile("input/2022/day03.txt")) { source => source.mkString }.get

}
