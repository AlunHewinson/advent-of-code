package aoc2017

import utils.utils._

object day9 extends App {
  // https://adventofcode.com/2017/day/9

  def removeCancelledCharacters(line: String): String = {
    "!.".r.replaceAllIn(line, "")
  }

  def removeGarbage(line: String): String = {
    "<[^>]*>".r.replaceAllIn(line, "")
  }
  def countGarbages(line: String): Int = {
    val emptyGarbages = "<[^>]*>".r.replaceAllIn(line, "<>")
    (emptyGarbages.length - removeGarbage(emptyGarbages).length) / 2
  }

  def removeNonBrackets(line: String): String = {
    "[^{}]".r.replaceAllIn(line, "")
  }

  def cumulateBrackets(line: String): Seq[Int] = {
    val toCumulate: Seq[Int] = line.split("").map {
      case "{" => 1
      case "}" => -1
      case _ => 0
    }.toSeq
    val cumulate = toCumulate.scanLeft(1)(_+_)
    cumulate.tail
  }
  def sumBrackets(line: String): Int = {
    val cumulate = cumulateBrackets(line)
    val bracks: Seq[String] = line.split("").toSeq

    bracks.indices.map(i => {
      if (bracks(i) == "}") cumulate(i)
      else 0
    }).sum
  }
  
  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test, 2017)
    val rowStrings: Seq[String] = "(\r*\n)".r.split(inp).toSeq

    val noCancelled = removeCancelledCharacters(rowStrings.head)
    val noGarbage = removeGarbage(noCancelled)
    val justBrackets = removeNonBrackets(noGarbage)
    val a1 = sumBrackets(justBrackets)

    val numberOfGarbages = countGarbages(noCancelled)
    val a2 = noCancelled.length - noGarbage.length - numberOfGarbages * 2

    (a1, a2)
  }
  println(solveDay(day = 9, test = true))
  println(solveDay(day = 9, test = false))
}
