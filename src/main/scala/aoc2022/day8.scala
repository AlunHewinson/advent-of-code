package aoc2022

import utils.utils.readDay

object day8 extends App {
  // https://adventofcode.com/2022/day/8

  def isFirstTallest(trees: IndexedSeq[Char]): Boolean = {
    if (trees.length == 1) true
    else trees.drop(1).forall(_ < trees(0))
  }
  def scene(trees: IndexedSeq[Char]): Int = {
    if (trees.length == 1) 0
    else {
      val blockingIndex = trees.drop(1).indexWhere(c => c >= trees(0))
      if (blockingIndex < 0) trees.drop(1).length
      else blockingIndex + 1
    }
  }

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test)
    val lines: Seq[String] = "\r*\n".r.split(inp).toSeq
    val trees: Seq[Seq[Char]] = lines.map(_.toCharArray)

    val rows: Int = lines.length - 1
    val columns = lines.head.length - 1

    val visible = (0 to rows).map(row => {
      (0 to columns).map(column => {
        val topDown = isFirstTallest((0 to row).map(rowlette => trees(rowlette)(column)).reverse)
        val bottomUp = isFirstTallest((row to rows).map(rowlette => trees(rowlette)(column)))
        val leftRight = isFirstTallest((0 to column).map(colette => trees(row)(colette)).reverse)
        val rightLeft = isFirstTallest((column to columns).map(colette => trees(row)(colette)))
        if (topDown | bottomUp | leftRight | rightLeft) 1 else 0
      })
    })
    val countVisible = visible.map(x => x.sum).sum

    val scenic = (0 to rows).map(row => {
      (0 to columns).map(column => {
        val topDown = scene((0 to row).map(rowlette => trees(rowlette)(column)).reverse)
        val bottomUp = scene((row to rows).map(rowlette => trees(rowlette)(column)))
        val leftRight = scene((0 to column).map(colette => trees(row)(colette)).reverse)
        val rightLeft = scene((column to columns).map(colette => trees(row)(colette)))
        //if (topDown | bottomUp | leftRight | rightLeft) 1 else 0
        topDown * bottomUp * leftRight * rightLeft
      })
    })
    //scenic.foreach(x => x.foreach(println))
    val maxScenic = scenic.map(x => x.max).max

    (countVisible, maxScenic)
  }
  println(solveDay(day = 8, test = true))
  println(solveDay(day = 8, test = false))
}
