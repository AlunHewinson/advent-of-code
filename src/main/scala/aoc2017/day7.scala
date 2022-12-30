package aoc2017

import utils.utils._

import scala.annotation.tailrec

object day7 extends App {
  // https://adventofcode.com/2017/day/7

  case class Bunch(flowers: Seq[Flower]) {
    def getFlowerByName(flowerName: String): Flower = {
      flowers.filter(_.name == flowerName).head
    }
    final def setPetalWeights(): Bunch = {
      this.copy(flowers = {
        flowers.map(flower => {
          val newPetalWeight: Seq[Int] = if (flower.petals.isEmpty) Seq(0)
          else {
            flower.petals.map(getWeight)
          }
          flower.copy(petalWeights = newPetalWeight)
        })
      })
    }
    final def getWeight(flowerName: String): Int = {
      val flower = getFlowerByName(flowerName)
      if (flower.petals.isEmpty) {
        flower.baseWeight
      } else {
        flower.baseWeight + flower.petals.map(petalName => getWeight(petalName)).sum
      }
    }
  }

  case class Flower(name: String, baseWeight: Int, petals: Seq[String], petalWeights: Seq[Int])

  def makeFlower(line: String): Flower = {
    line match {
      case s"$baseName ($baseWeight) -> $petals" => {
        Flower(baseName, baseWeight.toInt, petals.split(", "), Seq.empty)
      }
      case s"$baseName ($baseWeight)" => Flower(baseName, baseWeight.toInt, Seq.empty, Seq.empty)
    }
  }

  def solveDay(day: Int, test: Boolean = false): (String, Int) = {
    val inp: String = readDay(day, test, 2017)
    val rowStrings: Seq[String] = "(\r*\n)".r.split(inp).toSeq
    //rowStrings.foreach(println)

    val rootCandidates: Seq[String] = rowStrings.map(line => {
      line.split(" ")(0)
    })
    val notRoots: Seq[String] = rowStrings.flatMap(line => {
      val fromTo = line.split(" -> ")
      if (fromTo.length == 2) {
        fromTo(1).split(", ")
      } else None
    })
    val a1 = rootCandidates.diff(notRoots).head

    val weighted: Bunch = Bunch(rowStrings.map(makeFlower)).setPetalWeights()
    //weighted.flowers.foreach(println)
    val mismatches: Seq[Flower] = weighted.flowers.filter(flower => flower.petalWeights.distinct.length > 1) //.foreach(println)

    val minWeight: Int = mismatches.flatMap(flower => flower.petalWeights).min
    val flowerWithMinimumWeight: Flower = mismatches.filter(flower => flower.petalWeights contains minWeight).head
    val correctWeight: Int = flowerWithMinimumWeight.petalWeights.sorted.toSeq(1)
    val incorrectWeight: Int = flowerWithMinimumWeight.petalWeights.filter(weight => weight != correctWeight).head
    val correction = correctWeight - incorrectWeight
    val incorrectWeightIndex: Int = flowerWithMinimumWeight.petalWeights.indexOf(incorrectWeight)
    val incorrectFlowerName: String = flowerWithMinimumWeight.petals(incorrectWeightIndex)

    val a2 = weighted.getFlowerByName(incorrectFlowerName).baseWeight + correction

    (a1, a2)
  }
  println(solveDay(day = 7, test = true))
  println(solveDay(day = 7, test = false))
}
