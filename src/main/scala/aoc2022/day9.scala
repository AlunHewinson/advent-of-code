package aoc2022

import utils.utils.{Coord, readDay}

import scala.annotation.tailrec

object day9 extends App {
  // https://adventofcode.com/2022/day/9

  def knotDistance(knot1: Knot, knot2: Knot): Coord = {
    //(math.abs(knot1.pos._1 - knot2.pos._1), math.abs(knot1.pos._2 - knot2.pos._2))
    //(knot1.pos._1 - knot2.pos._1, knot1.pos._2 - knot2.pos._2)
    knot1.pos subtract knot2.pos
  }

  case class Knot(pos: Coord, pathsTaken: Seq[Coord]) {
    def move(moveVector: Coord): Knot = {
      //val newPos: Coord = (pos._1 + vector._1, pos._2 + vector._2)
      val newPos: Coord = pos add moveVector
      Knot(newPos, pathsTaken :+ newPos)
    }
    def mkHistory(): String = {
      this.pathsTaken.mkString(" -> ")
    }
    def printHistory(): Unit = {
      println(this.mkHistory())
    }
  }

  case class Rope(knots: Seq[Knot]) {

    @tailrec
    final def dragAll(instructions: Seq[String]): Rope = {
      if (instructions.isEmpty) this
      else {
        val dragged = this.drag(instructions.head)
        val tauted = dragged.taut()
        tauted.dragAll(instructions.tail)
      }
    }

    def drag(instruction: String): Rope = {
      instruction match {
        case s"U $distance" => dragUtil(Coord(0, 1), distance.toInt)
        case s"D $distance" => dragUtil(Coord(0, -1), distance.toInt)
        case s"L $distance" => dragUtil(Coord(-1, 0), distance.toInt)
        case s"R $distance" => dragUtil(Coord(1, 0), distance.toInt)
      }
    }

    @tailrec
    final def dragUtil(direction: Coord, distance: Int): Rope = {
      //println(s"dragUtil on distance $distance")
      if (distance == 0) this.taut()
      //else Rope(this.knots.head.move(direction) +: this.knots.drop(1)).drag(direction, distance - 1) //.taut()
      else this.moveWhich(0, direction).taut().dragUtil(direction, distance - 1)
    }

    def moveWhich(knotId: Int, direction: Coord): Rope = {
      Rope((this.knots.slice(0, knotId) :+ this.knots(knotId).move(direction)) ++ this.knots.slice(knotId + 1, 10000))
      //if (knotId == 0) Rope(this.knots.head.move(direction) +: this.knots.drop(1))
      //else if (knotId + 1 == this.knots.length) Rope((this.knots.slice(0, knotId) :+ this.knots(knotId).move(direction)) ++ this.knots.slice(knotId + 1, 10000))
      //else this
    }

    @tailrec
    private def taut(knotId: Int = 1): Rope = {
      //println(s"taut on knotId $knotId")
      if (knotId >= this.knots.length) this
      else {
        val slackness: Coord = knotDistance(this.knots(knotId), this.knots(knotId - 1))
        //println(slackness)
        val maxOffset: Int = math.max(math.abs(slackness.row), math.abs(slackness.column))
        //println(s"taut on maxOffset $maxOffset")
        if (maxOffset > 1) {
          this.moveWhich(knotId, Coord(-math.signum(slackness.row), -math.signum(slackness.column)))
        }.taut(knotId)
        else {
          //println(this.knots.map(knot => knot.pos))
          this.taut(knotId + 1)
        }
      }
    }
  }

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test)
    val lines: Array[String] = "\r*\n".r.split(inp).filterNot(_.isEmpty)
    //lines.foreach(println)

    val k1 = 1 to 2
    val rope1: Rope = Rope(k1.map(_ => Knot(Coord(0, 0), Seq(Coord(0, 0)))))
    val dragged1: Rope = rope1.dragAll(lines)
    //val histories1 = dragged1.knots.map(_.mkHistory())
    //histories1.foreach(println)
    val a1 = dragged1.knots.last.pathsTaken.distinct.length

    val k2 = 1 to 10
    val rope2: Rope = Rope(k2.map(_ => Knot(Coord(0, 0), Seq(Coord(0, 0)))))
    val dragged2: Rope = rope2.dragAll(lines)
    //val histories2 = dragged2.knots.map(_.mkHistory())
    //histories2.foreach(println)
    val a2 = dragged2.knots.last.pathsTaken.distinct.length

    //lines.foreach(println)

    (a1, a2)
  }
  println(solveDay(9, true))
  println(solveDay(9))

}
