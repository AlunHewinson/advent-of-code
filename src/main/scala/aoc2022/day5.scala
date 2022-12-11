package aoc2022

import utils.utils.readDay

object day5 extends App {
  // https://adventofcode.com/2022/day/5

  def moveOneByOne(stacks: Seq[Array[Char]], n: Int, fromStack: Int, toStack: Int): Seq[Array[Char]] = {
    val column: Array[Char] = stacks(fromStack - 1).slice(0, n).reverse
    val newStacks: Seq[Array[Char]] = Seq.range(0, stacks.length).map(i => {
      if (i == fromStack - 1) stacks(i).drop(n)
      else if (i == toStack - 1) column ++ stacks(i)
      else stacks(i)
    })
    newStacks
  }

  def moveInBulk(stacks: Seq[Array[Char]], n: Int, fromStack: Int, toStack: Int): Seq[Array[Char]] = {
    val column: Array[Char] = stacks(fromStack - 1).slice(0, n)
    val newStacks: Seq[Array[Char]] = Seq.range(0, stacks.length).map(i => {
      if (i == fromStack - 1) stacks(i).drop(n)
      else if (i == toStack - 1) column ++ stacks(i)
      else stacks(i)
    })
    newStacks
  }

  def moveOneByOneWrapper(stacks: Seq[Array[Char]], moveArray: Array[Int]): Seq[Array[Char]] = {
    moveOneByOne(stacks, moveArray(0), moveArray(1), moveArray(2))
  }
  def moveInBulkWrapper(stacks: Seq[Array[Char]], moveArray: Array[Int]): Seq[Array[Char]] = {
    moveInBulk(stacks, moveArray(0), moveArray(1), moveArray(2))
  }
  def solveDay(day: Int, test: Boolean = false): (String, String) = {
    val inp: String = readDay(day, test)

    val lines: Array[String] = "\r*\n".r.split(inp).filterNot(_.isEmpty) // stacks, base numbers, and moves
    val stackStrings: Array[String] = lines.filter(_ matches ".*\\[.*")
    val numberLines: Array[String] = lines.filter(_ matches "[ 0-9]+")
    val moves: Array[String] = lines.filter(_ matches "move.*")
    val parsedMoves: Array[Array[String]] = moves.map(move => "\\d+".r.findAllIn(move).toArray)
    val parsedMovesInt: Array[Array[Int]] = parsedMoves.map(x => x.map(y => y.toInt))

    val numbers: String = numberLines(0) // because there can only be one number line
    val numberOfStacks: Int = " +".r.split(numbers).reverse(0).toInt
    val stackNumbers: Seq[Int] = Seq.range(0, numberOfStacks)
    val stackColumns: Seq[Int] = stackNumbers.map(x => x * 4 + 1)

    val stacks: Seq[Array[Char]] = stackColumns.map(stackColumn => {
      stackStrings.map(stackString => stackString(stackColumn))
    })
    val stacksNoEmpties: Seq[Array[Char]] = stacks.map(x => x.filter(y => y.toString != " "))

    val stacksPart1: Seq[Array[Char]] = parsedMovesInt.foldLeft(stacksNoEmpties)(moveOneByOneWrapper)
    val stacksPart2: Seq[Array[Char]] = parsedMovesInt.foldLeft(stacksNoEmpties)(moveInBulkWrapper)

    (stacksPart1.map(stack => stack(0)).mkString, stacksPart2.map(stack => stack(0)).mkString)

  }
  println(solveDay(5, true))
  println(solveDay(5))

}
