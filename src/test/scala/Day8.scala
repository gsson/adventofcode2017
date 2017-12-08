import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source

object Day8 {
  case class Register(name: String) extends AnyVal

  sealed trait OpCode {
    def register: Register
    def arg: Int
  }

  object OpCode {
    def apply(op: String, register: Register, arg: Int): OpCode = {
      op.toLowerCase match {
        case "inc" => Inc(register, arg)
        case "dec" => Dec(register, arg)
      }
    }
  }

  final case class Inc(register: Register, arg: Int) extends OpCode
  final case class Dec(register: Register, arg: Int) extends OpCode

  sealed trait Condition {
    def register: Register
    def arg: Int
  }

  object Condition {
    def apply(cond: String, register: Register, arg: Int): Condition = {
      cond match {
        case ">"  => GT(register, arg)
        case ">=" => GE(register, arg)
        case "==" => EQ(register, arg)
        case "!=" => NE(register, arg)
        case "<=" => LE(register, arg)
        case "<"  => LT(register, arg)
      }
    }
  }

  final case class GT(register: Register, arg: Int) extends Condition
  final case class GE(register: Register, arg: Int) extends Condition
  final case class EQ(register: Register, arg: Int) extends Condition
  final case class NE(register: Register, arg: Int) extends Condition
  final case class LT(register: Register, arg: Int) extends Condition
  final case class LE(register: Register, arg: Int) extends Condition

  object Registers {
    def apply(): Registers = Registers(0, Map.empty[Register, Int])
  }
  case class Registers(historicMax: Int, registerValues: Map[Register, Int]) {
    private def get(register: Register): Int = registerValues.getOrElse(register, 0)
    private def updated(register: Register, value: Int) = {
      Registers(math.max(historicMax, value), registerValues.updated(register, value))
    }
    def delta(register: Register, amount: Int): Registers = {
      updated(register, get(register) + amount)
    }
    def compare(register: Register, amount: Int): Int = {
      Integer.compare(get(register), amount)
    }
  }

  object CondInterpreter {
    def apply(cond: Condition, registers: Registers): Boolean = {
      cond match {
        case GT(register, arg) => registers.compare(register, arg) >  0
        case GE(register, arg) => registers.compare(register, arg) >= 0
        case LT(register, arg) => registers.compare(register, arg) <  0
        case LE(register, arg) => registers.compare(register, arg) <= 0
        case EQ(register, arg) => registers.compare(register, arg) == 0
        case NE(register, arg) => registers.compare(register, arg) != 0
      }
    }
  }

  object OpCodeInterpreter {
    def apply(cond: OpCode, registers: Registers): Registers = {
      cond match {
        case Inc(register, arg) => registers.delta(register, arg)
        case Dec(register, arg) => registers.delta(register, -arg)
      }
    }
  }

  object InstructionInterpreter {
    def apply(instruction: Instruction, registers: Registers): Registers = {
      if (CondInterpreter(instruction.cond, registers)) {
        OpCodeInterpreter(instruction.opCode, registers)
      }
      else {
        registers
      }
    }
  }

  case class Instruction(opCode: OpCode, cond: Condition)

  @tailrec
  def evaluate(program: List[Instruction], registers: Registers): Registers = {
    program match {
      case instruction :: tail => evaluate(tail, InstructionInterpreter(instruction, registers))
      case Nil => registers
    }
  }

  def load(file: String): List[Instruction] = {
    val instruction = raw"""^(\w+) (\w+) (-?\d+) if (\w+) ([<>!=]+) (-?\d+)""".r
    Source.fromResource(file).getLines()
      .map {
        case instruction(opReg, op, opArg, condReg, cond, condArg) =>
          Instruction(OpCode(op, Register(opReg), opArg.toInt), Condition(cond, Register(condReg), condArg.toInt))
      }.toList
  }
}

class Day8A extends FlatSpec with Matchers {
  import Day8._
  def solver(instructions: List[Instruction]): Int = {
    evaluate(instructions, Registers()).registerValues.values.max
  }

  "Solver" should "print result" in {
    val instructions = load("Day8.txt")
    println(solver(instructions))
  }

  it should "pass test vectors" in {
    val instructions = load("Day8-test.txt")
    solver(instructions) should be(1)
  }
}

class Day8B extends FlatSpec with Matchers {
  import Day8._

  def solver(instructions: List[Instruction]): Int = {
    evaluate(instructions, Registers()).historicMax
  }

  "Solver" should "print result" in {
    val instructions = load("Day8.txt")
    println(solver(instructions))
  }

  it should "pass test vectors" in {
    val instructions = load("Day8-test.txt")
    solver(instructions) should be(10)
  }
}
