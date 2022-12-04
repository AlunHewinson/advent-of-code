package aoc2022

import scala.io.Source
import scala.util.Using

object day02 extends App {
  // https://adventofcode.com/2022/day/2

  val day02: String = Using(Source.fromFile("input/2022/day02.txt")) { source => source.mkString }.get

}
