package utils

import scala.io.Source
import scala.util.Using

object utils {
  def readDay(day: Int, test: Boolean = false): String = {
    val inputFile: String = s"input/2022/day$day${if (test) "test" else ""}.txt"
    Using(Source.fromFile(inputFile)) { source => source.mkString }.get
  }
}
