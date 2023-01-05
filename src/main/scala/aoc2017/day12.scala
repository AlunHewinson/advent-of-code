package aoc2017

import utils.utils._

import scala.annotation.tailrec

object day12 extends App {
  // https://adventofcode.com/2017/day/12

  case class Connection(from: Int, to: Seq[Int])
  case class Network(connections: Seq[Connection]) {
    def connectedTo(connected: Int = 0): Seq[Int] = {
      connectedTo(Seq(connected))
    }
    @tailrec
    final def connectedTo(connected: Seq[Int]): Seq[Int] = {
      val connectionMap: Map[Int, Seq[Int]] = this.connections.map(connection => connection.from -> connection.to).toMap
      val newConnected: Seq[Int] = (connected ++ connected.flatMap(connectionMap)).distinct
      if ((newConnected diff connected).isEmpty) connected
      else this.connectedTo(newConnected)
    }
    final def groupThem(groups: Boolean): Seq[Seq[Int]] = {
      this.groupThem(Seq(Seq.empty))
    }
    @tailrec
    final def groupThem(groups: Seq[Seq[Int]]): Seq[Seq[Int]] = {
      val usedNumbers: Seq[Int] = groups.flatten
      val availableIndices: Seq[Int] = this.connections.map(connection => connection.from)
      val unusedIndices = availableIndices diff usedNumbers
      if (unusedIndices.isEmpty) groups.tail
      else {
        val newGroup: Seq[Int] = this.connectedTo(unusedIndices.min)
        val newGroups: Seq[Seq[Int]] = groups :+ newGroup
        this.groupThem(newGroups)
      }
    }
  }

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test, 2017)
    val rowStrings: Seq[String] = "(\r*\n)".r.split(inp).toSeq

    val wires: Network = Network(rowStrings.map(line => {
      val fromTo: Seq[String] = line.replaceAll(" ", "").split("<->")
      val theFrom: Int = fromTo(0).toInt
      val theTos: Seq[Int] = fromTo(1).split(",").map(_.toInt)
      Connection(theFrom, theTos)
    }))

    val a1: Int = wires.connectedTo().length
    val a2 = wires.groupThem(false).length

    (a1, a2)
  }
  println(solveDay(day = 12, test = true))
  println(solveDay(day = 12, test = false))
}
