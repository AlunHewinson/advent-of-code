package aoc2022

import utils.utils.readDay

import scala.annotation.tailrec

object day20 extends App {
  // https://adventofcode.com/2022/day/20

  @tailrec
  def safeMod(x: BigInt, y: Int): BigInt = {
    if (x >= 0) x % y
    else safeMod((x % y) + y, y)
  }

  def shunt(fil: Vector[(Int, BigInt)], originalIndex: Int): Vector[(Int, BigInt)] = {
    val currentIndex: Int = fil.indexWhere(x => x._1 == originalIndex)
    val newIndex: BigInt = safeMod(currentIndex + fil(currentIndex)._2 - 1, fil.length - 1) + 1
    val movesToMake: Int = (newIndex - currentIndex).toInt
    val dropper: Seq[(Int, BigInt)] = Vector(fil(currentIndex))

    val newFil = movesToMake match {
      case 0 => fil
      case x if x < 0 => {
        val divvie = fil.splitAt(newIndex.toInt)
        (divvie._1 :+ fil(currentIndex)) ++ divvie._2.diff(dropper)
      }
      case _ => {
        val divvie = fil.splitAt((newIndex + 1).toInt)
        (divvie._1.diff(dropper) :+ fil(currentIndex)) ++ divvie._2
      }
    }
    newFil
  }
  @tailrec
  def shuntAll(fil: Vector[(Int, BigInt)], index: Int = 0): Vector[(Int, BigInt)] = {
    if (index >= fil.length) fil
    else {
      shuntAll(shunt(fil, index), index + 1)
    }
  }
  @tailrec
  def mix(fil: Vector[(Int, BigInt)], times: Int = 1): Vector[(Int, BigInt)] = {
    if (times == 0) fil
    else mix(shuntAll(fil), times - 1)
  }

  def solveDay(day: Int, test: Boolean = false): (BigInt, BigInt) = {
    val inp: String = readDay(day, test)
    val elements: Vector[BigInt] = "\r*\n".r.split(inp).map(x => BigInt(x)).toVector

    // Part 1
    val indexedElements: Vector[(Int, BigInt)] = elements.indices.map(i => (i, elements(i))).toVector
    val decrypted = mix(indexedElements)

    val indexOf0 = decrypted.indexWhere(_._2 == 0)
    val oneThousandth = decrypted((indexOf0 + 1000) % decrypted.length)._2
    val twoThousandth = decrypted((indexOf0 + 2000) % decrypted.length)._2
    val threeThousandth = decrypted((indexOf0 + 3000) % decrypted.length)._2

    // Part 2
    val bigElements: Seq[BigInt] = elements.map(x => x * BigInt("811589153"))
    val bigIndexedElements: Vector[(Int, BigInt)] = bigElements.indices.map(i => (i, bigElements(i))).toVector
    val bigDecrypted = mix(bigIndexedElements, 10)

    val bigIndexOf0 = bigDecrypted.indexWhere(_._2 == 0)
    val bigOneThousandth = bigDecrypted((bigIndexOf0 + 1000) % bigDecrypted.length)._2
    val bigTwoThousandth = bigDecrypted((bigIndexOf0 + 2000) % bigDecrypted.length)._2
    val bigThreeThousandth = bigDecrypted((bigIndexOf0 + 3000) % bigDecrypted.length)._2

    (oneThousandth + twoThousandth + threeThousandth, bigOneThousandth + bigTwoThousandth + bigThreeThousandth)
  }
  println(solveDay(day = 20, test = true))
  println(solveDay(day = 20, test = false))
}
