package aoc2017

import utils.utils._

import scala.annotation.tailrec

object day6 extends App {
  // https://adventofcode.com/2017/day/6

  def findLargestBank(banks: Seq[Int]): Int = {
    banks.zipWithIndex.maxBy(_._1)._2
  }

  @tailrec
  def addToBanks(banks: Seq[Int], indices: Seq[Int]): Seq[Int] = {
    if (indices.isEmpty) banks
    else {
      val newBanks = banks.zipWithIndex.map {
        case (v, i) if i == indices.head => v + 1
        case (v, i)                      => v
      }
      addToBanks(newBanks, indices.tail)
    }
  }

  def redistribute(banks: Seq[Int]): Seq[Int] = {
    val largest = findLargestBank(banks)
    val toRedistribute = banks(largest)
    val indicesToIncrement: Seq[Int] = Range.inclusive(largest + 1, largest + toRedistribute).
      map(index => index % banks.length)

    val zeroedLargest = banks.zipWithIndex.map {
      case (v, i) if i == largest => 0
      case (v, i)                 => v
    }

    val newBanks = addToBanks(zeroedLargest, indicesToIncrement)
    newBanks
  }

  @tailrec
  def redistributeCycle(banks: Seq[Int], previousBanks: Seq[String] = Seq.empty): Seq[String] = {
    val bankString: String = banks.mkString("|")
    if (previousBanks.contains(bankString)) previousBanks :+ bankString
    else redistributeCycle(redistribute(banks), previousBanks :+ bankString)
  }

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test, 2017)
    val banks: Seq[Int] = "(\r*\n)".r.split(inp).toSeq.head.split("[ \t]+").map(_.toInt).toSeq

    val bankStrings = redistributeCycle(banks)
    val repeatedBank: String = bankStrings.last
    val banksIndices: Seq[Int] = bankStrings.dropRight(1).zipWithIndex map {
      case (bank, i) if bank == repeatedBank => i
      case (bank, i)                         => 0
    }

    (bankStrings.length - 1, bankStrings.length - banksIndices.max - 1)
  }
  println(solveDay(day = 6, test = true))
  println(solveDay(day = 6, test = false))
}
