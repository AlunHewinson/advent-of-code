package aoc2017

import utils.utils._

import scala.annotation.tailrec

object day5 extends App {
  // https://adventofcode.com/2017/day/5

  @tailrec
  def visitInstructions(instructions: Seq[Int], index: Int = 0, part2: Boolean = false, movesTaken: Int = 0): Int = {
    if (index < 0) movesTaken
    else if (index >= instructions.length) movesTaken
    else {
      val newIndex = index + instructions(index)
      val newInstructionsPart1 = instructions.indices.map {
        case i if i == index => instructions(i) + 1
        case i               => instructions(i)
      }
      val newInstructionsPart2 = instructions.indices.map {
        case i if i == index => if (instructions(i) >= 3) instructions(i) - 1 else instructions(i) + 1
        case i               => instructions(i)
      }
      val newInstructions = if (part2) newInstructionsPart2 else newInstructionsPart1
      visitInstructions(newInstructions, newIndex, part2 = part2, movesTaken = movesTaken + 1)
    }
  }

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test, 2017)
    val instructions: Seq[Int] = "(\r*\n)".r.split(inp).toSeq.map(_.toInt)

    val a1: Int = visitInstructions(instructions)
    val a2: Int = visitInstructions(instructions, part2 = true)

    (a1, a2)
  }
  println(solveDay(day = 5, test = true))
  println(solveDay(day = 5, test = false))
}
