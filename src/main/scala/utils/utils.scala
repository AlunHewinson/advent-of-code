package utils

import scala.io.Source
import scala.util.Using

object utils {
  def readDay(day: Int, test: Boolean = false): String = {
    val inputFile: String = s"input/2022/day$day${if (test) "test" else ""}.txt"
    Using(Source.fromFile(inputFile)) { source => source.mkString }.get
  }

  case class Lens[A, B](
                         get: A => B,
                         set: (A, B) => A
                       )
  def compose[Outer, Inner, Value](
                                    outer: Lens[Outer, Inner],
                                    inner: Lens[Inner, Value]
                                  ) = Lens[Outer, Value](
    get = outer.get andThen inner.get,
    set = (obj, value) => outer.set(obj, inner.set(outer.get(obj), value))
  )
}
