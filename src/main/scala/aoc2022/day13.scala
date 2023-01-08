package aoc2022

import utils.utils.readDay
import scala.annotation.tailrec
import scala.util.parsing.json._

object day13 extends App {
  // https://adventofcode.com/2022/day/13

  def isOrdered(pair: String): Boolean = {
    val pairLeft: String = "(\r*\n)".r.split(pair)(0)
    val pairRight: String = "(\r*\n)".r.split(pair)(1)
    checkElementwise(parseJsonList(pairLeft), parseJsonList(pairRight))
  }

  @tailrec
  def checkElementwise(l: List[Any], r: List[Any]): Boolean = {
    if (l.isEmpty) true
    else if (r.isEmpty) false
    else {
      (l.head, r.head) match {
        case (lDouble: Double, rDouble: Double) => {
          if (lDouble < rDouble) true
          else if (lDouble > rDouble) false
          else checkElementwise(l.tail, r.tail)
        }
        case (lDouble: Double, rList: List[Any]) => checkElementwise(List(lDouble), rList)
        case (lList: List[Any], rDouble: Double) => checkElementwise(lList, List(rDouble))
        case (lList: List[Any], rList: List[Any]) if (lList.isEmpty && rList.isEmpty) => checkElementwise(l.tail, r.tail)
        case (lList: List[Any], rList: List[Any]) => checkElementwise(lList, rList)
        case _ => {
          throw new Exception("Unexpected pairing")
        }
      }
    }
  }

  def parseJsonList(jsonString: String): List[Any] = {
    JSON.parseFull(jsonString) match {
      case Some(list: List[Any]) => list
      case _ => Nil
    }
  }

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test)
    val pairs: Seq[String] = "(\r*\n){2}".r.split(inp).toSeq

    val orderedOnly: Seq[Boolean] = pairs.map(isOrdered)
    val trueIndices: Seq[Int] = orderedOnly.zipWithIndex.filter(pair => pair._1).map(pair => pair._2 + 1)
    val a1 = trueIndices.sum

    val monoStrings: Seq[String] = "(\r*\n)+".r.split(inp).toSeq ++ Seq("[[2]]", "[[6]]")
    val monoJsons = monoStrings.map(parseJsonList)
    val sortedJsons = monoJsons.sortWith(checkElementwise)
    val a2 = sortedJsons.zipWithIndex.filter(pair => Seq(List(List(2.0)), List(List(6.0))).contains(pair._1)).map(pair => pair._2 + 1).product //.foreach(println)

    (a1, a2)
  }
  println(solveDay(day = 13, test = true))
  println(solveDay(day = 13, test = false))
}
