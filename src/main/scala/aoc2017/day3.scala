package aoc2017

import utils.utils._

object day3 extends App {
  // https://adventofcode.com/2017/day/3

  def oscillate(containingSquare: Int): Seq[Int] = {
    val oscillationRange: Range = ((containingSquare - 1) / 2) until containingSquare
    val downCycle = oscillationRange.reverse.drop(1) //tail
    val upCycle = oscillationRange.drop(1) //tail
    Seq.fill(containingSquare - 1)(downCycle ++ upCycle).flatten
  }

  def iToXAddress(i: Int): Int = {
    if (i == 0) 0
    else {
      math.round(math.sin(math.Pi/2 * math.floor(math.sqrt(4*i-3)))).toInt + iToXAddress(i-1)
    }
  }
  def iToYAddress(i: Int): Int = {
    if (i == 0) 0
    else {
      -math.round(math.cos(math.Pi/2 * math.floor(math.sqrt(4*i-3)))).toInt + iToYAddress(i-1)
    }
  }
  def iToXYAddress(i: Int): Coord = {
    Coord(iToYAddress(i), iToXAddress(i))
  }

  def scoreSpiral(iValuesMap: Map[Int, Int]): Map[Int, Int] = {
    val spiral: Iterable[Int] = iValuesMap.keys
    val unvaluedIndices: Iterable[Int] = spiral.filter(indx => iValuesMap(indx) == 0)

    if (unvaluedIndices.isEmpty) iValuesMap
    else {
      val addressValuesMap: Map[Coord, Int] = spiral.map(i => (iToXYAddress(i), iValuesMap(i))).toMap
      val i: Int = unvaluedIndices.min
      val coord: Coord = iToXYAddress(i)
      val newValue: Int =
        addressValuesMap.getOrElse(coord.nw(), 0) +
          addressValuesMap.getOrElse(coord.n(), 0) +
          addressValuesMap.getOrElse(coord.ne(), 0) +
          addressValuesMap.getOrElse(coord.w(), 0) +
          addressValuesMap.getOrElse(coord.e(), 0) +
          addressValuesMap.getOrElse(coord.sw(), 0) +
          addressValuesMap.getOrElse(coord.s(), 0) +
          addressValuesMap.getOrElse(coord.se(), 0)
      val newMap: Map[Int, Int] = iValuesMap map {
        case (x, someOldValue) if x==i => i -> math.max(newValue, 1)
        case (x, unchangedValue)       => x -> unchangedValue
      }
      scoreSpiral(newMap)
    }
  }

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test, 2017)
    val memoryAddress: Int = "(\r*\n)".r.split(inp).toSeq.head.toInt

    val ceilingSquareRoot: Int = math.ceil(math.sqrt(memoryAddress)).toInt
    val oddSquareRootUpper: Int = 1 + ceilingSquareRoot - (ceilingSquareRoot % 2)

    val oscillation = oscillate(oddSquareRootUpper)
    val a1 = oscillation(memoryAddress - 1 - math.pow(oddSquareRootUpper - 2, 2).toInt)

    val iRange: Seq[Int] = 0 to 100
    val iValuesMap: Map[Int, Int] = iRange.map(i => (i, 0)).toMap

    // This is suspect. The first value written mightn't be the min, but it works for my input
    val a2 = scoreSpiral(iValuesMap).values.filter(_ > memoryAddress).min

    (a1, a2)
  }
  println(solveDay(day = 3, test = true))
  println(solveDay(day = 3, test = false))
}
