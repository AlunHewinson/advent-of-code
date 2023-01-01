package aoc2017

import utils.utils._

object day8 extends App {
  // https://adventofcode.com/2017/day/8

  case class Register(addressValue: Map[String, Int])

  case class Registry(registers: Map[String, Int], maxValue: Int = 0) {
    def instruct(instructions: Seq[String]): Registry = {
      if (instructions.isEmpty) this
      else {
        val instructionOne = instructions.head

        val modifiedRegistry = instructionOne match {
          case s"$reg1 $operator $operand if $reg2 $comparator $comparee" => {
            val conditionMet: Boolean = {
              comparator match {
                case "==" => this.registers(reg2) == comparee.toInt
                case ">" => this.registers(reg2) > comparee.toInt
                case "<" => this.registers(reg2) < comparee.toInt
                case ">=" => this.registers(reg2) >= comparee.toInt
                case "<=" => this.registers(reg2) <= comparee.toInt
                case "!=" => this.registers(reg2) != comparee.toInt
                case _ => false
              }
            }
            def operateOn(op: String): (Int, Int) => Int = {
              op match {
                case "inc" => (a: Int, b: Int) => a + b
                case "dec" => (a: Int, b: Int) => a - b
                case _ => (a: Int, b: Int) => a
              }
            }
            val registerReplacement = this.registers map {
              case (someReg, addressValue) if conditionMet & someReg == reg1 => someReg -> operateOn(operator)(addressValue, operand.toInt)
              case x => x
            }
           val maxRegister: Int = registerReplacement.values.max
            Registry(registerReplacement, math.max(this.maxValue, maxRegister))
          }
          case _ => this
        }

        modifiedRegistry.instruct(instructions.tail)
      }
    }
  }

  def findRegisters(lines: Seq[String]): Seq[String] = {
    lines.flatMap {
      case s"$reg1 $operator $operand if $reg2 $comparator $comparee" => Seq(reg1, reg2)
      case _ => Seq.empty
    }.distinct
  }

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {
    val inp: String = readDay(day, test, 2017)
    val rowStrings: Seq[String] = "(\r*\n)".r.split(inp).toSeq

    val registers: Seq[String] = findRegisters(rowStrings)
    val initialRegistry = Registry(registers.map(registry => registry -> 0).toMap)

    val instructed: Registry = initialRegistry.instruct(rowStrings)
    val a1 = instructed.registers.values.max
    val a2 = instructed.maxValue
    (a1, a2)
  }
  println(solveDay(day = 8, test = true))
  println(solveDay(day = 8, test = false))
}
