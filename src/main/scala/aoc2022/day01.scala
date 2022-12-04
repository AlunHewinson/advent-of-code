package aoc2022

import scala.io.Source
import scala.util.Using

object day01 extends App {
  // https://adventofcode.com/2022/day/1

  val day01: String = Using(Source.fromFile("input/2022/day01.txt")) { source => source.mkString }.get
  val elfSeparator: String = "\n\n"
  val itemSeparator: String = "\n"
  val elves: Array[String] = elfSeparator.r.split(day01)
  val snacks: Array[Int] = elves.map(elf => {
    val stringItems: Array[String] = itemSeparator.r.split(elf)
    stringItems.map(_.toInt).sum
  })
  println(snacks.max)
  println(snacks.sorted(Ordering.Int.reverse).take(3).sum)

}
