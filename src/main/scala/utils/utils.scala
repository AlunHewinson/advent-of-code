package utils

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object utils {
  def readDay(day: Int, test: Boolean = false, year: Int = 2022): String = {
    val inputFile: String = s"input/$year/day$day${if (test) "test" else ""}.txt"
    Using(Source.fromFile(inputFile)) { source => source.mkString }.get
  }

  case class Lens[A, B](get: A => B, set: (A, B) => A)

  def compose[Outer, Inner, Value](
                                    outer: Lens[Outer, Inner],
                                    inner: Lens[Inner, Value]
                                  ): Lens[Outer, Value] = Lens[Outer, Value](
    get = outer.get andThen inner.get,
    set = (obj, value) => outer.set(obj, inner.set(outer.get(obj), value))
  )

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (b==0) a.abs else gcd(b, a%b)
  }

  def lcm(a: Int, b: Int): Int = {
    (a*b).abs/gcd(a,b)
  }

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
    def distance(other: Coord): Coord = Coord(math.abs(this.row - other.row), math.abs(this.column - other.column))
    def manhattenMagnitude(): Int = this.row + this.column
  }


}
