package aoc2022

import utils.utils.readDay

object day7 extends App {
  // https://adventofcode.com/2022/day/7

  def gotoParent(wd: String): String = {

    "^/[^/]+".r.replaceFirstIn(wd.reverse, "").reverse
  }

  def cd(wd: String, cdCommand: String): String = {
    wd + "[^ ]+$".r.findFirstIn(cdCommand).getOrElse("") + "/"
  }

  def calculateWorkingDirectory(workingDirectory: String, command: String): String = {
    command match {
      case "$ cd /" => "/"
      case "$ cd .." => gotoParent(workingDirectory)
      case "$ ls" => workingDirectory
      case _ if ("\\$ cd .*".r.matches(command)) => cd(workingDirectory, command)
      case _ => workingDirectory
    }
  }
  def accumulateWds(wds: Array[String], command: String): Array[String] = {
    val yo = calculateWorkingDirectory(wds.last, command)
    wds :+ yo
  }

  def splitDirIntoSubs(dir: String): Array[String] = {
    if (dir == "/") Array(dir)
    else Array(dir) ++ splitDirIntoSubs(gotoParent(dir))
  }

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test)
    val lines: Array[String] = "\r*\n".r.split(inp).filterNot(_.isEmpty)
    val workingDirectory: String = ""

    val wds: Array[String] = lines.foldLeft(Array(workingDirectory))(accumulateWds).drop(1)
    val fileSizes = lines.map(line => {
      "^\\d+".r.findFirstIn(line).getOrElse("0").toInt
    })

    val dirSizes = wds.zip(fileSizes)
    val subdirSizes = dirSizes.flatMap(dir => splitDirIntoSubs(dir._1).map(subdir => (subdir, dir._2)))
    val aggregatedSizes = subdirSizes.groupBy(_._1).mapValues(_.map(_._2).sum)
    val part1Answer = aggregatedSizes.filter(_._2 <= 100000).values.sum

    val rootSize: Int = aggregatedSizes("/")
    val part2Answer = aggregatedSizes.filter(_._2 >= rootSize - 40000000).values.min
    (part1Answer, part2Answer)
  }
  println(solveDay(7, true))
  println(solveDay(7))

}
