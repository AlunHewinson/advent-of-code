package aoc2017

import utils.utils._
import scala.annotation.tailrec

object day2 extends App {
  // https://adventofcode.com/2017/day/2

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test, 2017)
    val rowStrings: Seq[String] = "(\r*\n)".r.split(inp).toSeq

    val rows = rowStrings.map(row => {
      row.split("[ \t]+").map(_.toInt)
    })

    val a1 = rows.map(values => values.max - values.min)

    val a2 = rows.map(row => {
      val combos = row.combinations(2)
      combos.map(toVals => if (toVals.max % toVals.min == 0) toVals.max / toVals.min else 0).sum
    })

    (a1.sum, a2.sum)
  }
  println(solveDay(day = 2, test = true))
  println(solveDay(day = 2, test = false))
}
