package aoc2022

import utils.utils.readDay
import scala.annotation.tailrec

object day23 extends App {
  // https://adventofcode.com/2022/day/23

  val emptyGround: Char = ".".toCharArray.head
  val elf: Char = "#".toCharArray.head
  //val noNeighbours: Seq[Boolean] = Seq(false, false, false, false, false, false, false, false)

  //val dup = List(1,1,1,2,3,4,5,5,6,100,101,101,102)
  //println(dup.groupBy(identity).collect { case (x, List(_,_,_*)) => x })

  case class Row(plots: Seq[Char]) {
    def squish(): String = {
      this.plots.mkString("")
    }
  }

  case class Neighbours(nw: Boolean = false, n: Boolean = false, ne: Boolean = false,
                        w: Boolean = false, me: Boolean = true, e: Boolean = false,
                        sw: Boolean = false, s: Boolean = false, se: Boolean = false)

  case class Coord(row: Int, column: Int) {
    def nw(): Coord = Coord(this.row - 1, this.column - 1)
    def n():  Coord = Coord(this.row - 1, this.column + 0)
    def ne(): Coord = Coord(this.row - 1, this.column + 1)
    def w():  Coord = Coord(this.row + 0, this.column - 1)
    def e():  Coord = Coord(this.row + 0, this.column + 1)
    def sw(): Coord = Coord(this.row + 1, this.column - 1)
    def s():  Coord = Coord(this.row + 1, this.column + 0)
    def se(): Coord = Coord(this.row + 1, this.column + 1)
    def add(other: Coord): Coord = Coord(this.row + other.row, this.column + other.column)
    def subtract(other: Coord): Coord = Coord(this.row - other.row, this.column - other.column)
    def p(): Unit = println(this.row.toString + "," + this.column.toString)
    def mk(): String = this.row.toString + "," + this.column.toString
  }

  case class Elf(position: Coord, proposedPosition: Coord, neighbours: Neighbours = Neighbours()) {
    def setProposedPosition(preference: Seq[Int]): Elf = {
      //println(preference)
      val goNowhere: Option[Coord] = if(neighbours.nw | neighbours.n | neighbours.ne | neighbours.e | neighbours.se | neighbours.s | neighbours.sw | neighbours.w) None else Some(Coord(0, 0))
      val goNorth: Option[Coord] = if(neighbours.nw | neighbours.n | neighbours.ne) None else Some(Coord(-1, 0))
      val goEast:  Option[Coord] = if(neighbours.ne | neighbours.e | neighbours.se) None else Some(Coord(0, 1))
      val goSouth: Option[Coord] = if(neighbours.sw | neighbours.s | neighbours.se) None else Some(Coord(1, 0))
      val goWest:  Option[Coord] = if(neighbours.nw | neighbours.w | neighbours.sw) None else Some(Coord(0, -1))
      val preferredMoves = preference map Seq(goNorth, goEast, goSouth, goWest)
      //println(preference map Seq("N", "E", "S", "W"))
      //println(goNowhere +: preferredMoves :+ Some(Coord(0, 0)))
      //println((goNowhere +: preferredMoves :+ Some(Coord(0, 0))).flatten.headOption.get)
      this.copy(proposedPosition = position add (goNowhere +: preferredMoves :+ Some(Coord(0, 0))).flatten.headOption.get)
    }
    def resetProposedPosition(): Elf = {
      this.copy(proposedPosition = position)
    }
    def move(): Elf = {
      this.copy(position = proposedPosition)
    }
  }

  case class Field(rows: Seq[Row], preference: Seq[Int], elves: Seq[Elf] = Seq.empty) {
    //def addRowAbove(): Field = this.copy(rows = Row(Seq.fill(this.rows.head.plots.length)(emptyGround)) +: rows)
    //def addRowBelow(): Field = this.copy(rows = rows :+ Row(Seq.fill(this.rows.head.plots.length)(emptyGround)))
    //def addPlotsLeft(): Field = this.copy(rows = this.rows.map(row => Row(emptyGround +: row.plots)), this.preference)
    //def addPlotsRight(): Field = this.copy(rows = this.rows.map(row => Row(row.plots :+ emptyGround)), this.preference)
    def show(): Unit = println(this.rows.map(_.squish()).mkString("\n"))
    def debugShow(): Field = {
      println(this.rows.map(_.squish()).mkString("\n"))
      this
    }
    def rotatePreference(): Field = this.copy(preference = this.preference.drop(1) :+ this.preference.head)
    def setElves(): Field = {
      this.copy(elves =
      this.rows.zipWithIndex.collect { case(r, i) =>
        r.plots.zipWithIndex.collect { case(p, j) if p == elf => Elf(Coord(i, j), Coord(i, j)) }
      }.flatten)
    }
    //def setRowsFromElves(activeElves: Seq[Elf] = this.elves, rows: Seq[Row] = Seq.empty): Field = {
    def setRowsFromElves(activeElves: Int = 0, rows: Seq[Row] = Seq.empty): Field = {
      if (activeElves >= this.elves.length) this.copy(rows = rows)
      else {
        val allPositions: Seq[Coord] = this.elves.map(_.position)
        val rowMin = allPositions.map(coord => coord.row).min
        val rowMax = allPositions.map(coord => coord.row).max
        val colMin = allPositions.map(coord => coord.column).min
        val colMax = allPositions.map(coord => coord.column).max

        val toSubtract = Coord(rowMin, colMin)
        allPositions.map(coord => coord subtract toSubtract)

        val coordToSet = this.elves(activeElves).position subtract toSubtract

        val fieldToElfify = if (rows.isEmpty) {
          (0 to (rowMax - rowMin)).map(row => {
            Row((0 to (colMax - colMin)).map(col => {
              emptyGround
            }))
          })
        } else rows

        //val rowToSet = emptyField(coordToSet.row).plots
        //val tst = emptyField(coordToSet.row).plots. //(coordToSet.column)
        val elfifiedField = fieldToElfify.updated(
          coordToSet.row, Row(fieldToElfify(coordToSet.row).plots.updated(coordToSet.column, elf))
        )

        this.copy(rows = fieldToElfify).setRowsFromElves(activeElves + 1, elfifiedField)

      }
    }
    def setProposedPositions(): Field = {
      this.copy(elves = this.elves.map(_.setProposedPosition(this.preference)))
    }
    def moveElves(): Field = {
      this.copy(elves = this.elves.map(_.move()))
    }
    def discardClashingProposals(): Field = {
      val proposedPositions: Seq[Coord] = this.elves.map(_.proposedPosition)
      val duplicateProposals: Seq[Coord] = proposedPositions.groupBy(identity).collect { case (x, Seq(_,_,_*)) => x }.toSeq

      this.copy(elves =
        this.elves.map(elf => if (duplicateProposals.contains(elf.proposedPosition)) elf.resetProposedPosition() else elf)
      )
      //this.copy(elves = this.elves.map(elf => elf.setProposedPosition(this.preference)))
      //this
    }
    def setNeighbours(): Field = {
      val coordinates: Seq[Coord] = this.elves.map(elf => elf.position)
      val elvesWithNeighbours = this.elves.map(elf => elf.copy(neighbours = Neighbours(
        coordinates.contains(elf.position.nw()),
        coordinates.contains(elf.position.n()),
        coordinates.contains(elf.position.ne()),
        coordinates.contains(elf.position.w()),
        me = true,
        coordinates.contains(elf.position.e()),
        coordinates.contains(elf.position.sw()),
        coordinates.contains(elf.position.s()),
        coordinates.contains(elf.position.se()),
      )))
      this.copy(elves = elvesWithNeighbours)
    }
    @tailrec
    final def playRounds(n: Int): Field = {
      print(n); print(" ")
      if (n <= 0) this
      else {
        // add empty borders
        //val widerField = this.addRowAbove().addRowBelow().addPlotsRight().addPlotsLeft()
        val widerField = this

        // proposal round
        //   get all elves
        //   for each elf
        //     count neighbours
        //     find proposed direction (if any)
        //     calculate proposed position
        val proposeField = widerField.setElves().setNeighbours().setProposedPositions().discardClashingProposals()
        //println("proposals:")
        //proposeField.elves.foreach(elf => println(elf.position.mk() + " -> " + elf.proposedPosition.mk()))

        // move round
        //   filter out move clashes
        //   move the elves
        val anyToMove: Boolean = proposeField.elves.exists(elf => elf.position != elf.proposedPosition)
        if (anyToMove) {
          val moveField = proposeField.moveElves()
          //   set rows from elf positions
          val setField = moveField.setRowsFromElves()
          // update preferred direction
          setField.rotatePreference().playRounds(n - 1)
        } else {
          println(s"no moves now in round $n")
          proposeField.rotatePreference().playRounds(0)
        }
      }
    }
  }

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test)
    val rowStrings: Seq[String] = "(\r*\n)".r.split(inp).toSeq
    val preference = Seq(0, 2, 3, 1)
    val field = Field(rowStrings.map(r => Row(r.toCharArray)), preference)

    //field.addPlotsRight().show()
    //val neighbourinos = field.setElves().setNeighbours()
    //neighbourinos.elves.foreach(println)

    val played = field.playRounds(10000)
    val allPositions: Seq[Coord] = played.elves.map(_.position)
    val rowMin = allPositions.map(coord => coord.row).min
    val rowMax = allPositions.map(coord => coord.row).max
    val colMin = allPositions.map(coord => coord.column).min
    val colMax = allPositions.map(coord => coord.column).max

    played.show()
    val emptyFields: Int = (1 + rowMax - rowMin) * (1 + colMax - colMin) - played.elves.length

    (emptyFields, 98)
  }
  println(solveDay(day = 23, test = true))
  println(solveDay(day = 23, test = false))
}
