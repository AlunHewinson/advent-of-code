package aoc2022

import utils.utils.{Coord, readDay}

object day10 extends App {
  // https://adventofcode.com/2022/day/10

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test)
    val lines: Array[String] = "\r*\n".r.split(inp).filterNot(_.isEmpty)

    val adders: Seq[Int] = lines.flatMap {
      case s"addx $addMe" => Seq(0, addMe.toInt)
      case _ => Seq(0)
    }
    val cumulativeSum: Seq[(Int, Int)] = (0 +: adders).scanLeft(1)(_+_).zipWithIndex
    val interestingSignalStrengths: Seq[(Int, Int)] = cumulativeSum.filter { case (v, i) => (i % 40) == 20 }
    val answer1: Int = interestingSignalStrengths.map { case (v, i) => v * i }.sum
    println(cumulativeSum)

    val lit = cumulativeSum.drop(1).dropRight(1).map(sprite => {
      if (math.abs(sprite._1 - ((sprite._2 - 1) % 40)) < 2) "#" else "."
    })
    (0 to 5).foreach(i => println(lit.slice(i * 40, i * 40 + 40).mkString("")))



    (answer1, 98)
  }
  println(solveDay(10, true))
  println(solveDay(10))

}
