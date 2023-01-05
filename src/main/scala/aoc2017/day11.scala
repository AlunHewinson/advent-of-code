package aoc2017

import utils.utils._

object day11 extends App {
  // https://adventofcode.com/2017/day/11

  def hexOffset(offsetString: String): (Int, Int) = {
    offsetString match {
      case "n" => (2, 0)
      case "s" => (-2, 0)
      case "ne" => (1, 1)
      case "nw" => (1, -1)
      case "se" => (-1, 1)
      case "sw" => (-1, -1)
    }
  }

  def addHexCoordinates(hexOffset1: (Int, Int), hexOffset2: (Int, Int)): (Int, Int) = {
    (hexOffset1._1 + hexOffset2._1, hexOffset1._2 + hexOffset2._2)
  }

  def hexDistance(hexPoint: (Int, Int)) = {
    (math.abs(hexPoint._1) - math.abs(hexPoint._2)) / 2 + math.abs(hexPoint._2)
  }

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test, 2017)
    val rowStrings: Seq[String] = "(\r*\n)".r.split(inp).toSeq.head.split(",")

    val currentHex = rowStrings.map(hexOffset).foldLeft((0, 0))(addHexCoordinates) //foreach(println)
    val a1 = hexDistance(currentHex)

    val scanHex = rowStrings.map(hexOffset).scanLeft((0, 0))(addHexCoordinates)
    val a2 = scanHex.map(hexDistance).max

    (a1, a2)
  }
  println(solveDay(day = 11, test = true))
  println(solveDay(day = 11, test = false))
}
