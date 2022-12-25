package aoc2022

import utils.utils._

import scala.annotation.tailrec

object day25 extends App {
  // https://adventofcode.com/2022/day/25

  val two = "2".toCharArray.head
  val one = "1".toCharArray.head
  val zero = "0".toCharArray.head
  val minusOne = "-".toCharArray.head
  val minusTwo = "=".toCharArray.head

  @tailrec
  def magnitude(x: BigInt, accumulateMagnitude: Int = 0): Int = {
    if (x < 1) accumulateMagnitude + 1
    else magnitude(x / 5, accumulateMagnitude + 1)
  }

  def baseSnafu(x: BigInt) = {
    val mag = magnitude(x)
    val indics = (1 to mag).map(m => {
        (x + BigInt(5).pow(m)/2)./(BigInt(5).pow(m-1)).mod(5)
    })
    (indics.map(_.toInt) map Seq("=", "-", "0", "1", "2")).reverse.mkString("")
  }

  def solveDay(day: Int, test: Boolean = false): String = {
    val inp: String = readDay(day, test)
    val lines: Seq[String] = "\r*\n".r.split(inp).filterNot(_.isEmpty).toSeq

    val a1Decimal = lines.map(x => {
      val numerals: Seq[BigInt] = x.map { y => {
        if (y==two) BigInt(2)
        else if (y==one) BigInt(1)
        else if (y==zero) BigInt(0)
        else if (y==minusOne) BigInt(-1)
        else if (y==minusTwo) BigInt(-2)
        else BigInt(98)
      }}
      numerals.indices.map(i => {
        numerals.reverse(i) * (BigInt(5).pow(i))
      }).sum
    }).sum
    baseSnafu(a1Decimal)
  }
  println(solveDay(25, true))
  println(solveDay(25))

}
