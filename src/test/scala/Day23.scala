import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source

object Day23 {
  object Instruction {
    val OpNames = Vector("Set", "Sub", "Mul", "Jnz")

    val Set = 0
    val Sub = 1
    val Mul = 2
    val Jnz = 3

    def apply(encoded: Long): Instruction = new Instruction(encoded)
    def apply(op: Int, arg1Reg: Boolean, reg: Int, arg2Reg: Boolean, arg: Int) = {
      val encoded =
        ((op.toLong & 0x0fffL) << 48) |
          ((reg.toLong & 0xffffL) << 32) |
          (arg.toLong & 0xffffffffL) |
          (if (arg1Reg) 0x8000000000000000L else 0) |
          (if (arg2Reg) 0x4000000000000000L else 0)
      new Instruction(encoded)
    }
    def apply(op: Int, arg1: Arg, arg2: Arg): Instruction = {
      Instruction(op, arg1.isReg, arg1.arg, arg2.isReg, arg2.arg)
    }

  }
  class Instruction(val encoded: Long) extends AnyVal {
    def arg1Reg = (encoded & 0x8000000000000000L) != 0
    def arg2Reg = (encoded & 0x4000000000000000L) != 0
    def op = ((encoded >>> 48) & 0x0fff).toInt
    def arg1 = ((encoded >>> 32) & 0xffff).toInt
    def arg2 = encoded.toInt

    def opName = Instruction.OpNames(op)
    def arg1Type = if (arg1Reg) s"      ${(arg1 + 'a').toChar}" else f"$arg1%7d"
    def arg2Type = if (arg2Reg) s"      ${(arg2 + 'a').toChar}" else f"$arg2%7d"

    override def toString: String = s"${opName} ${arg1Type} ${arg2Type}"
  }

  implicit def instructionToLong(instruction: Instruction): Long = instruction.encoded
  implicit def longToInstruction(encoded: Long): Instruction = Instruction(encoded)

  case class Program(instructions: Array[Long]) extends AnyVal {
    def apply(i: Int): Instruction = Instruction(instructions(i))
  }
  case class Arg(isReg: Boolean, arg: Int)

  object CpuState {
    def cpuState(): CpuState[Long] = new Part1CpuState()
  }

  abstract class CpuState[OUT]() {
    val registers: Array[Long] = new Array[Long](8)
    var pc: Int = 0
    var op: Int = -1
    var reg1: Int = -1
    var reg2: Int = -1
    var arg1: Long = -1L
    var arg2: Long = -1L

    def loadInstruction(program: Program): Unit = {
      load(program.apply(pc))
    }

    def load(instruction: Instruction) = {
      op = instruction.op
      if (instruction.arg1Reg) {
        arg1 = registers(instruction.arg1)
        reg1 = instruction.arg1
      }
      else {
        arg1 = instruction.arg1
      }
      if (instruction.arg2Reg) {
        arg2 = registers(instruction.arg2)
        reg2 = instruction.arg2
      }
      else {
        arg2 = instruction.arg2
      }
    }

    def set: Unit = { registers.update(reg1, arg2); pc += 1 }
    def sub: Unit = { registers.update(reg1, arg1 - arg2); pc += 1 }
    def mul: Unit = { registers.update(reg1, arg1 * arg2); pc += 1 }
    def jnz: Unit = { val r = if (arg1 != 0) arg2.toInt else 1; pc += r }

    def step(program: Program): OUT = ???

    def shouldReturn(output: OUT): Boolean = ???

    def run(program: Program): OUT = {
      @tailrec
      def continuously(last: OUT): OUT = {
        if (shouldReturn(last)) {
          last
        }
        else {
          continuously(step(program))
        }
      }
      continuously(step(program))
    }

    override def toString: String = {
      s"pc: $pc, registers:\n  ${registers.zipWithIndex.map(v => s"${(v._2 + 'a').toChar}: ${v._1}").mkString("\n  ")}"
    }
  }

  class Part1CpuState() extends CpuState[Long] {
    var muls: Long = 0L

    override def shouldReturn(output: Long): Boolean = output != -1


    override def step(program: Program): Long = {
      val opc = pc
      loadInstruction(program)
      op match {
        case Instruction.Set => set
        case Instruction.Sub => sub
        case Instruction.Mul => mul; muls += 1
        case Instruction.Jnz => jnz
      }
      if (opc < 11 || opc > 20) {
        println(f"${opc}%2d ${program(opc)}%s ${registers.map(r => f"$r%7d").mkString(" ")}%s")
      }

      if (pc >= program.instructions.length || pc < 0) muls else -1L
    }
  }

  val ROp = raw"(\w+)\s+([a-z])".r
  val IOp = raw"(\w+)\s+(-?[0-9]+)".r
  val IIOp = raw"(\w+)\s+(-?[0-9]+)\s+(-?[0-9]+)".r
  val RIOp = raw"(\w+)\s+([a-z])\s+(-?[0-9]+)".r
  val IROp = raw"(\w+)\s+(-?[0-9]+)\s+([a-z])".r
  val RROp = raw"(\w+)\s+([a-z])\s+([a-z])".r

  def dc(): Arg = Arg(false, -1)
  def lit(arg: String): Arg = Arg(false, arg.toInt)
  def lit(arg: Int): Arg = Arg(false, arg)
  def reg(arg: String): Arg = Arg(true, arg(0).toInt - 'a')

  def load(file: String): Program = {
    import Instruction._

    def parseOp(op: String): Int = op match {
      case "set" => Set
      case "sub" => Sub
      case "mul" => Mul
      case "jnz" => Jnz
    }

    val instructions = Source.fromResource(file)
      .getLines()
      .map {
        case IOp(op, arg1) => Instruction(parseOp(op), lit(arg1), dc())
        case ROp(op, arg1) => Instruction(parseOp(op), reg(arg1), dc())
        case IIOp(op, arg1, arg2) => Instruction(parseOp(op), lit(arg1), lit(arg2))
        case RIOp(op, arg1, arg2) => Instruction(parseOp(op), reg(arg1), lit(arg2))
        case IROp(op, arg1, arg2) => Instruction(parseOp(op), lit(arg1), reg(arg2))
        case RROp(op, arg1, arg2) => Instruction(parseOp(op), reg(arg1), reg(arg2))
      }
      .map(_.encoded)
      .toArray
    Program(instructions)
  }

}

class Day23A extends FlatSpec with Matchers {
  import Day23._
  def solve(program: Program): Long = {
    CpuState.cpuState().run(program)
  }

  "Solver" should "print result" in {
    val p = load("Day23.txt")

    println(solve(p))
  }

  it should "pass test vectors" in {
  }
}


class Day23B extends FlatSpec with Matchers {
  def isPrime(n: Int): Boolean = {
    Range(2, Math.sqrt(n).toInt + 1)
        .exists(n % _ == 0)
  }

  "Solver" should "print result" in {
    val a = Range.inclusive(109300, 126300, 17)
      .count(isPrime)
    println(a)
  }

}