package aoc2022

import utils.utils.readDay
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

object day21 extends App {
  // https://adventofcode.com/2022/day/21

//  val commands: Seq[String] = Seq(
//    """def root() = spqr() + espn()""",
//    """def spqr() = 73""",
//    """def espn() = 25""",
//    """println(root())"""
//  )
//  val superCommands: String = commands.mkString("\n |    ")
//
//  val toolbox = currentMirror.mkToolBox()
//  val source =
//    s"""
//       |object HelloWorld {
//       |  def main(args: Array[String]): Unit = {
//       |    $superCommands
//       |  }
//       |}
//       |
//       |HelloWorld.main(Array())
//       |""".stripMargin

  def makeFunctionString(inputString: String): String = {
    inputString match {
      case s"$funcName: $otherVar1 $operator $otherVar2" => {
        s"def $funcName(): BigInt = $otherVar1() $operator $otherVar2()"
      }
      case s"$funcName: $nummer" => {
        s"def $funcName(): BigInt = BigInt($nummer)"
      }
    }
  }
  def makeFunctionStringRoot(inputString: String): String = {
    inputString match {
      case s"root: $otherVar1 $operator $otherVar2" => {
        s"def root(): Boolean = $otherVar1() == $otherVar2()"
      }
      case s"$funcName: $otherVar1 $operator $otherVar2" => {
        s"def $funcName(): BigInt = $otherVar1() $operator $otherVar2()"
      }
      case s"$funcName: $nummer" => {
        s"""def $funcName(): BigInt = BigInt("$nummer")"""
      }
    }
  }

  def makeSource(monctions: Seq[String]): String = {
    val superMonction: String = monctions.mkString("\n |    ")
    s"""
         |object HelloWorld {
         |  def main(args: Array[String]): Unit = {
         |    $superMonction
         |  }
         |}
         |
         |HelloWorld.main(Array())
         |""".stripMargin
  }

  val tst: String = "root: lccz + pttp"

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test)
    val toolbox = currentMirror.mkToolBox()
    val monkeyStrings: Seq[String] = "\r*\n".r.split(inp).toSeq

    val monctions1: Seq[String] = monkeyStrings.map(makeFunctionString) :+ "println(root())"
    val source1 = makeSource(monctions1)
    println("part 1:")
    toolbox.compile(toolbox.parse(source1))()

    println("part 2:")
    val part2Answers = Seq(BigInt("301"), BigInt("3916491093817"))
    //for (newNumber <- BigInt("3916491093817") to BigInt("3916491093817")) { // I had to iterate towards this manually :(
    for (newNumber <- part2Answers) {
      //println(newNumber)
      val humanStrings = monkeyStrings.map(x => {
        val regex = "(humn: )[0-9]+".r
        regex.replaceAllIn(x, "$1" + newNumber)
      })
      val monctions2: Seq[String] = humanStrings.map(makeFunctionStringRoot) :+ "if (root()) println(humn())"
      //val monctions2: Seq[String] = humanStrings.map(makeFunctionStringRoot) :+ "println(lccz() - pttp())"
      val source2 = makeSource(monctions2)
      toolbox.compile(toolbox.parse(source2))()
    }
    (98, 98)
  }
  solveDay(day = 21, test = true)
  solveDay(day = 21, test = false)
}
