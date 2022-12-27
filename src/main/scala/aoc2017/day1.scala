package aoc2017

import utils.utils._
import scala.annotation.tailrec

object day1 extends App {
  // https://adventofcode.com/2017/day/1

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test, 2017)
    val rowStrings: Seq[String] = "(\r*\n)".r.split(inp).toSeq
    val captchas: Array[Int] = rowStrings.head.split("").map(_.toInt)
    val pairs: Iterator[Int] = (captchas :+ captchas.head).sliding(2).map(a => if (a(0) == a(1)) a(0) else 0)

    val podes = captchas.slice(0, captchas.length/2)
    val antipodes = captchas.slice(captchas.length/2, captchas.length)
    //podes.foreach(println)
    //antipodes.foreach(println)
    val antiPairs = podes.indices.map {i =>
      println(s"${podes(i)} <-> ${antipodes(i)}")
      if (podes(i) == antipodes(i)) 2*podes(i) else 0
    }

    (pairs.sum, antiPairs.sum)
  }
  println(solveDay(day = 1, test = true))
  println(solveDay(day = 1, test = false)) //622 too low
}
