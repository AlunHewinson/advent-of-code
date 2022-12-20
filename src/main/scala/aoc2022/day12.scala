package aoc2022

import utils.utils.readDay

import scala.:+
import scala.annotation.tailrec
import scala.language.postfixOps
import scala.math._

object day12 extends App {
  // https://adventofcode.com/2022/day/6

  val magicS: Char = 83
  val magicE: Char = 69
  val magica: Char = 97

  def parseTopography(inp: String): (Array[Array[Char]], Array[Array[Int]]) = {
    val lines: Array[String] = "\r*\n".r.split(inp).filterNot(_.isEmpty)
    //lines.foreach(println)
    val letterMatrix: Array[Array[Char]] = lines.map(x => x.toCharArray)
    val heightMatrix: Array[Array[Int]] = letterMatrix.map(row => {
      row.map {
        case 83 => 1 // magic number! 83 is S
        case 69 => 26 // magic number! 69 is E
        case column => column.toInt - 96
      }
    })
    (letterMatrix, heightMatrix)
  }
  def findValuesInMatrix[T](matrix: Array[Array[T]], value: T): Array[(Int, Int)] = {
    var accumulator: Array[(Int, Int)] = Array.empty
    for (row <- matrix.indices) {
      for (column <- matrix(row).indices) {
        if (matrix(row)(column) == value) accumulator = accumulator :+ (row, column)
      }
    }
    accumulator
  }
  def reachableFromNeighbour(myHeight: Int, neighbourHeight: Int): Boolean = {
    myHeight - neighbourHeight < 2
  }
  def findNeighboursWhoCanReachMe(heightMatrix: Array[Array[Int]], rowColumn: (Int, Int)): Array[(Int, Int)] = {
    val row: Int = rowColumn._1
    val column: Int = rowColumn._2

    val emptyLocationArray: Array[(Int, Int)] = Array.empty

    val yUp = if (row == 0) emptyLocationArray else {
      if(reachableFromNeighbour(heightMatrix(row)(column), heightMatrix(row-1)(column))) Array((row-1, column))
      else emptyLocationArray
    }
    val xLeft = if (column == 0) emptyLocationArray else {
      if(reachableFromNeighbour(heightMatrix(row)(column), heightMatrix(row)(column-1))) Array((row, column-1))
      else emptyLocationArray
    }
    val yDown = if (row == heightMatrix.indices.last) emptyLocationArray else {
      if(reachableFromNeighbour(heightMatrix(row)(column), heightMatrix(row+1)(column))) Array((row+1, column))
      else emptyLocationArray
    }
    val xRight = if (column == heightMatrix(row).indices.last) emptyLocationArray else {
      if(reachableFromNeighbour(heightMatrix(row)(column), heightMatrix(row)(column+1))) Array((row, column+1))
      else emptyLocationArray
    }

    yUp ++ xLeft ++ yDown ++ xRight
  }
  def setNeighbourHeights(matrix: Array[Array[Int]], neighbours: Array[(Int, Int)], value: Int): Array[Array[Int]] = {
    val loop = neighbours.length
    @tailrec
    def setNeighbourHeightsUtil(matrix: Array[Array[Int]], neighbours: Array[(Int, Int)], value: Int, loop: Int): Array[Array[Int]] = {
      if (loop < 1) matrix
      else {
        if (matrix(neighbours(loop - 1)._1)(neighbours(loop - 1)._2) < 0) matrix(neighbours(loop - 1)._1)(neighbours(loop - 1)._2) = value
        setNeighbourHeightsUtil(matrix, neighbours, value, loop - 1)
      }
    }
    setNeighbourHeightsUtil(matrix, neighbours, value, loop)
  }
  def extendPathEnds(heightMatrix: Array[Array[Int]], distanceMatrix: Array[Array[Int]], pathEnds: Array[(Int, Int)], maxDistance: Int): Array[Array[Int]] = {
    val loop = pathEnds.length
    @tailrec
    def extendPathEndsUtil(heightMatrix: Array[Array[Int]], distanceMatrix: Array[Array[Int]], pathEnds: Array[(Int, Int)], maxDistance: Int, loop: Int): Array[Array[Int]] = {
      if (loop < 1) distanceMatrix
      else {
        val reachingNeighbours = findNeighboursWhoCanReachMe(heightMatrix, pathEnds(loop - 1))
        val upgradedNeighbours = setNeighbourHeights(distanceMatrix, reachingNeighbours, maxDistance + 1)
        extendPathEndsUtil(heightMatrix, upgradedNeighbours, pathEnds, maxDistance, loop - 1)
      }
    }
    extendPathEndsUtil(heightMatrix, distanceMatrix, pathEnds, maxDistance, loop)
  }
  @tailrec
  def populateDistances(heightMatrix: Array[Array[Int]], distanceMatrix: Array[Array[Int]], start: (Int, Int)): Array[Array[Int]] = {
    val isStartDistanceSet = distanceMatrix(start._1)(start._2) >= 0
    if (isStartDistanceSet) distanceMatrix
    else {
      val maxDistance: Int = distanceMatrix.map(row => row.max).max
      val pathEnds: Array[(Int, Int)] = findValuesInMatrix(distanceMatrix, maxDistance)
      val extendedPaths: Array[Array[Int]] = extendPathEnds(heightMatrix, distanceMatrix, pathEnds, maxDistance)
      populateDistances(heightMatrix, extendedPaths, start)
    }
  }

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test)
    val matrices = parseTopography(inp)

    val letterMatrix = matrices._1
    val heightMatrix = matrices._2
    val start: (Int, Int) = findValuesInMatrix(letterMatrix, magicS)(0)
    val end: (Int, Int) = findValuesInMatrix(letterMatrix, magicE)(0)
    val distanceMatrix: Array[Array[Int]] = letterMatrix.map(row => {
      row.map { _ => -1 }
    })
    distanceMatrix(end._1)(end._2) = 0

    /* GENERAL PRINTS */
    //letterMatrix.foreach(x => println(x.mkString("Array(", ", ", ")")))
    //heightMatrix.foreach(x => println(x.mkString("Array(", ", ", ")")))
    //println(start)
    //println(end)
    //distanceMatrix.foreach(x => println(x.mkString("Array(", ", ", ")")))

    val part1Grid = populateDistances(heightMatrix, distanceMatrix, start)

    val multiStarts = findValuesInMatrix(letterMatrix, magica) :+ start
    val part2Distances = multiStarts.map(starter => {
      part1Grid(starter._1)(starter._2)
    })
    val part2Minimum = part2Distances.map {
      case -1 => 10000
      case other => other
    }.min

    //part1Grid.foreach(x => println(x.mkString("Array(", ", ", ")")))
    (part1Grid(start._1)(start._2), part2Minimum)

  }

  println(solveDay(12, true))
  println(solveDay(12))
}