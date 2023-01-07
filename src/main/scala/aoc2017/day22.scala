package aoc2017

import utils.utils._

import scala.annotation.tailrec

object day22 extends App {
  // https://adventofcode.com/2017/day/22

  val clean = ".".toCharArray.head
  val dirty = "#".toCharArray.head
  val weak  = "W".toCharArray.head
  val flag  = "F".toCharArray.head

  case class Node(x: Int, y: Int) {
    def move(direction: Int) = {
      direction match {
        case 0 => Node(this.x, this.y - 1)
        case 1 => Node(this.x + 1, this.y)
        case 2 => Node(this.x, this.y + 1)
        case 3 => Node(this.x - 1, this.y)
      }
    }
  }
  case class Cluster(infectedNodes: Seq[Node], direction: Int = 0, cursor: Node = Node(0, 0), infectionCount: Int = 0,
                     weakenedNodes: Seq[Node] = Seq.empty, flaggedNodes: Seq[Node] = Seq.empty) {
    def show(): Unit = {
      val weaknesses: Seq[Node] = this.weakenedNodes
      val infections: Seq[Node] = this.infectedNodes
      val flagnesses: Seq[Node] = this.flaggedNodes
      val maxX = (infections ++ weaknesses ++ flagnesses).map(node => node.x).max
      val minX = (infections ++ weaknesses ++ flagnesses).map(node => node.x).min
      val maxY = (infections ++ weaknesses ++ flagnesses).map(node => node.y).max
      val minY = (infections ++ weaknesses ++ flagnesses).map(node => node.y).min

      (minY to maxY).map {y =>
        (minX to maxX).map {x =>
          if (weaknesses.contains(Node(x, y))) weak
          else if (infections.contains(Node(x, y))) dirty
          else if (flagnesses.contains(Node(x, y))) flag
          else clean
        }.mkString(" ")
      }.foreach(println)
    }
    @tailrec
    final def work(bursts: Int = 1): Cluster = {
      if (bursts <= 0) this
      else {
        val infections = this.infectedNodes
        val (newInfections, newDirection, newInfectionCount) = if (infections.contains(cursor)) {
          (
            infections diff Seq(this.cursor),
            (this.direction + 1) % 4,
            this.infectionCount
          )
        } else {
          (
            infections :+ this.cursor,
            (this.direction + 3) % 4,
            this.infectionCount + 1
          )
        }
        val newCursor = cursor.move(newDirection)
        Cluster(newInfections, newDirection, newCursor, newInfectionCount).work(bursts - 1)
      }
    }
    @tailrec
    final def weakWork(bursts: Int = 1): Cluster = {
      if (bursts % 10000 == 0) println(bursts)
      if (bursts <= 0) this
      else {
        val infections = this.infectedNodes
        val weaknesses = this.weakenedNodes
        val flagnesses = this.flaggedNodes
        val (newDirection, newInfectionCount, newWeakened, newInfections, newFlagged) =
          if (weaknesses.contains(cursor)) {
            // we're on a weakened node --> infected
            (
              this.direction,
              this.infectionCount + 1,
              weaknesses diff Seq(this.cursor), // newWeakened
              infections :+ this.cursor, // newInfections
              flagnesses // newFlagged
            )
          } else if (infections.contains(cursor)) {
            // we're on an infected node --> flagged
            (
              (this.direction + 1) % 4,
              this.infectionCount,
              weaknesses, // newWeakened
              infections diff Seq(this.cursor), // newInfections
              flagnesses :+ this.cursor // newFlagged
            )
          } else if (flagnesses.contains(cursor)) {
          // we're on a flagged node --> clean
          (
            (this.direction + 2) % 4,
            this.infectionCount,
            weaknesses, // newWeakened
            infections, // newInfections
            flagnesses diff Seq(this.cursor) // newFlagged
          )
          } else {
            // we're on a clean node --> weakened
            (
              (this.direction + 3) % 4,
              this.infectionCount,
              weaknesses :+ this.cursor, // newWeakened
              infections, // newInfections
              flagnesses // newFlagged
            )
          }
        val newCursor = cursor.move(newDirection)
        Cluster(newInfections, newDirection, newCursor, newInfectionCount, newWeakened, newFlagged).weakWork(bursts - 1)
      }
    }
  }

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test, 2017)
    val rowStrings: Seq[String] = "(\r*\n)".r.split(inp).toSeq

    val gridMiddle = (rowStrings.length - 1) / 2

    val infected: Cluster = Cluster(rowStrings.zipWithIndex.flatMap {
      case (nodeRow, row) => nodeRow.zipWithIndex.flatMap {
        case (chr, col) if chr==dirty => Some(Node(col, row))
        case _ => None
      }
    },  cursor = Node(gridMiddle, gridMiddle))
    val pandemic = infected.work(10000)
    //pandemic.show()
    val a1 = pandemic.infectionCount

    val workdemic = infected.weakWork(10000000)
    //workdemic.show()
    val a2 = workdemic.infectionCount

    (a1, a2)
  }
  println(solveDay(day = 22, test = true))
  println(solveDay(day = 22, test = false))
}
