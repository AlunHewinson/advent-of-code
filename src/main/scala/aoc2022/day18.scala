package aoc2022

import utils.utils.readDay
import java.lang.reflect.Constructor
import scala.annotation.tailrec

object day18 extends App {
  // https://adventofcode.com/2022/day/18

//  val lava: Int = 1
//  val air: Int = 2
//  val water: Int = 3

  case class Grain(x: Int = -1, y: Int = -1, z: Int = -1) {
    val cs = this.getClass.getConstructors
    def createFromList(params: List[Any]): Grain =
      cs(0).newInstance(params map { _.asInstanceOf[AnyRef] } : _*).asInstanceOf[Grain]
  }

  case class Volume(lavas: Seq[Grain], airs: Seq[Grain], waters: Seq[Grain], xRange: Range, yRange: Range, zRange: Range) {
    def isAir(grain: Grain): Boolean = {
      this.airs.contains(grain)
    }
    def isLava(grain: Grain): Boolean = {
      this.lavas.contains(grain)
    }
    def isWater(grain: Grain): Boolean = {
      this.waters.contains(grain)
    }
    def setAir(): Volume = {
      val newAirs = xRange.flatMap ( x =>
        yRange.flatMap(y =>
          zRange.map(z =>
            Grain(x, y, z)
          )
        )
      )
      this.copy(airs = newAirs diff this.lavas)
    }

    val r: Range = 2 to 5
    val tst: Boolean = r contains 9

    @tailrec
    final def dunk(waterSeeds: Seq[Grain]): Volume = {
      if (waterSeeds.isEmpty) this
      else {
        val newWaterSeeds: Seq[Grain] = waterSeeds.flatMap {
          waterSeed => {
            generateNeighbours(waterSeed).
              distinct.
              filter(isAir).
              filter(this.xRange contains _.x).
              filter(this.yRange contains _.y).
              filter(this.zRange contains _.z)
          }
        }.distinct
        val wetVolume = this.
          copy(waters = waters ++ newWaterSeeds).
          copy(airs = airs diff newWaterSeeds)
        wetVolume.dunk(newWaterSeeds)
      }
    }
  }

  def generateNeighbours(seed: Grain): Seq[Grain] = {
    Seq(
      Grain(seed.x - 1, seed.y, seed.z),
      Grain(seed.x + 1, seed.y, seed.z),
      Grain(seed.x, seed.y - 1, seed.z),
      Grain(seed.x, seed.y + 1, seed.z),
      Grain(seed.x, seed.y, seed.z - 1),
      Grain(seed.x, seed.y, seed.z + 1)
    )
  }
  def isNeighbour(grain1: Grain, grain2: Grain): Boolean = {
    val xDiff: Int = math.abs(grain1.x - grain2.x)
    val yDiff: Int = math.abs(grain1.y - grain2.y)
    val zDiff: Int = math.abs(grain1.z - grain2.z)
    xDiff + yDiff + zDiff == 1
  }
  def isInBomb(grain1: Array[Int], grain2: Array[Int]): Boolean = {
    val xDiff = math.abs(grain1(0) - grain2(0))
    val yDiff = math.abs(grain1(1) - grain2(1))
    val zDiff = math.abs(grain1(2) - grain2(2))
    xDiff + yDiff + zDiff == 0
  }
  def countAllNeighbours(bomb: Seq[Grain], grain: Grain): Int = {
    bomb.map(x => if (isNeighbour(x, grain)) 1 else 0).sum
  }
  def countWaterNeighbours(volume: Volume): Int = {
    volume.lavas.map(lava => {
      val waterNeighbours = generateNeighbours(lava)
      waterNeighbours.intersect(volume.waters).length
    }).sum
  }
  def immerse(volume: Volume): Volume = {
    volume
  }

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test)
    val bomb: Seq[Grain] = "\r*\n".r.split(inp).map(x => {
      val xList = x.split(",").map(_.toInt).toList
      Grain().createFromList(xList)
    })
    val neighbours = bomb.map(x => countAllNeighbours(bomb, x))

    val xMin: Int = bomb.map(g => g.x).min
    val xMax: Int = bomb.map(g => g.x).max
    val yMin: Int = bomb.map(g => g.y).min
    val yMax: Int = bomb.map(g => g.y).max
    val zMin: Int = bomb.map(g => g.z).min
    val zMax: Int = bomb.map(g => g.z).max
    val xRange: Range = xMin - 1 to xMax + 1
    val yRange: Range = yMin - 1 to yMax + 1
    val zRange: Range = zMin - 1 to zMax + 1

    val air: Seq[Grain] = Seq.empty
    val pond: Seq[Grain] = Seq.empty

    val volume: Volume = Volume(lavas = bomb, air, pond, xRange, yRange, zRange).setAir()
    val waterSeed: Seq[Grain] = Seq(Grain(xRange.min, yRange.min, zRange.min))
    val wetted: Volume = volume.dunk(waterSeed)
    val wetCount: Int = countWaterNeighbours(wetted)

    (bomb.length * 6 - neighbours.sum, wetCount)
  }
  println(solveDay(18, true))
  println(solveDay(18, false))
}
