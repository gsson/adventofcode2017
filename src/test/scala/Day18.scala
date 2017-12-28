import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.{switch, tailrec}
import scala.collection.mutable
import scala.io.Source

object Day18 {
  object Instruction {
    val OpNames = Vector("Set", "Add", "Mul", "Mod", "Snd", "Rcv", "Jgz")

    val Set = 0
    val Add = 1
    val Mul = 2
    val Mod = 3

    val Snd = 4
    val Rcv = 5
    val Jgz = 6

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
    def arg1Type = if (arg1Reg) (arg1 + 'a').toChar.toString else if (arg1 == -1) "" else arg1.toString
    def arg2Type = if (arg2Reg) (arg2 + 'a').toChar.toString else if (arg2 == -1) "" else arg2.toString

    override def toString: String = s"${opName} ${arg1Type} ${arg2Type}"
  }

  implicit def instructionToLong(instruction: Instruction): Long = instruction.encoded
  implicit def longToInstruction(encoded: Long): Instruction = Instruction(encoded)

  case class Program(instructions: Array[Long]) extends AnyVal {
    def apply(i: Int): Instruction = Instruction(instructions(i))
  }
  case class Arg(isReg: Boolean, arg: Int)

  object CpuState {
    def part1(): CpuState[Long] = new Part1CpuState()
    def part2(p: Long, inQueue: mutable.Queue[Long], outQueue: mutable.Queue[Long]): CpuState[StepStatus] = new Part2CpuState(p, inQueue, outQueue)
  }

  abstract class CpuState[OUT]() {
    val registers: Array[Long] = new Array[Long](25)
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
    def add: Unit = { registers.update(reg1, arg1 + arg2); pc += 1 }
    def mul: Unit = { registers.update(reg1, arg1 * arg2); pc += 1 }
    def mod: Unit = { registers.update(reg1, arg1 % arg2); pc += 1 }
    def jgz: Unit = { val r = if (arg1 > 0) arg2.toInt else 1; pc += r }

    def snd: OUT = ???
    def rcv: OUT = ???

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
    var playing: Long = 0L
    override def snd: Long = { playing = arg1; pc += 1; -1 }
    override def rcv: Long = { val r = if (arg1 != 0) playing else -1; pc += 1; r }

    override def shouldReturn(output: Long): Boolean = output != -1L

    override def step(program: Program): Long = {
      loadInstruction(program)
      op match {
        case Instruction.Set => set; -1L
        case Instruction.Add => add; -1L
        case Instruction.Mul => mul; -1L
        case Instruction.Mod => mod; -1L
        case Instruction.Jgz => jgz; -1L
        case Instruction.Snd => snd
        case Instruction.Rcv => rcv
      }
    }
  }


  sealed trait StepStatus {
    def sent: Int
  }
  case object Ok extends StepStatus {
    def sent = -1
  }
  case class NeedInput(sent: Int) extends StepStatus

  class Part2CpuState(p: Long, inQueue: mutable.Queue[Long], outQueue: mutable.Queue[Long]) extends CpuState[StepStatus] {
    registers.update('p' - 'a', p)
    var sndCount = 0
    override def snd: StepStatus = {
      outQueue.enqueue(arg1)
      sndCount += 1
      pc += 1
      Ok
    }
    override def rcv: StepStatus = {
      if (inQueue.isEmpty) {
        NeedInput(sndCount)
      }
      else {
        registers.update(reg1, inQueue.dequeue())
        pc += 1
        Ok
      }
    }

    override def shouldReturn(output: StepStatus): Boolean = output != Ok

    override def step(program: Program): StepStatus = {
      loadInstruction(program)
      op match {
        case Instruction.Set => set; Ok
        case Instruction.Add => add; Ok
        case Instruction.Mul => mul; Ok
        case Instruction.Mod => mod; Ok
        case Instruction.Jgz => jgz; Ok
        case Instruction.Snd => snd
        case Instruction.Rcv => rcv
      }
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
      case "add" => Add
      case "mul" => Mul
      case "mod" => Mod
      case "snd" => Snd
      case "rcv" => Rcv
      case "jgz" => Jgz
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

class Day18A extends FlatSpec with Matchers {
  import Day18.Instruction._
  import Day18._

  def solver(program: Program): Long = {
    CpuState.part1().run(program)
  }

  "Solver" should "print result" in {
    val program = load("Day18.txt")
    println(solver(program))
  }

  "Solver" should "pass test vectors" in {
    val program = Program(Array(
      Instruction(Set, reg("a"), lit(1)),
      Instruction(Add, reg("a"), lit(2)),
      Instruction(Mul, reg("a"), reg("a")),
      Instruction(Mod, reg("a"), lit(5)),
      Instruction(Snd, reg("a"), dc()),
      Instruction(Set, reg("a"), lit(0)),
      Instruction(Rcv, reg("a"), dc()),
      Instruction(Jgz, reg("a"), lit(-1)),
      Instruction(Set, reg("a"), lit(1)),
      Instruction(Jgz, reg("a"), lit(-2))
    ))

    solver(program) should be(4)
  }
}

class Day18B extends FlatSpec with Matchers {
  import Day18.Instruction._
  import Day18._

  def solver(program: Program): Long = {
    val pid0In = mutable.Queue[Long]()
    val pid1In = mutable.Queue[Long]()

    val pid0 = CpuState.part2(0, pid0In, pid1In)
    val pid1 = CpuState.part2(1, pid1In, pid0In)


    @tailrec
    def iterate(r0: StepStatus, r1: StepStatus): Int = {
      println(r0, r1)
      (r0, r1) match {
        case (NeedInput(_), NeedInput(result)) if pid0In.isEmpty && pid1In.isEmpty => result
        case (_, _) =>
          val nextR0 = pid0.run(program)
          val nextR1 = pid1.run(program)
          iterate(nextR0, nextR1)
      }
    }

    val r0 = pid0.run(program)
    val r1 = pid1.run(program)
    iterate(r0, r1)
  }

  "Solver" should "print result" in {
    val program = load("Day18.txt")
    println(solver(program))
  }

  "Solver" should "pass test vectors" in {
    val program = Program(Array(
      Instruction(Snd, lit(1), dc()),
      Instruction(Snd, lit(2), dc()),
      Instruction(Snd, reg("p"), dc()),
      Instruction(Rcv, reg("a"), dc()),
      Instruction(Rcv, reg("b"), dc()),
      Instruction(Rcv, reg("c"), dc()),
      Instruction(Rcv, reg("d"), dc())
    ))

    solver(program) should be(3)
  }
}
