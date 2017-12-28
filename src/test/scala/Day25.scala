import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source

object Day25 {
  object State {
    def apply(state: String): State = new State(state(0) - 'A')
  }

  case class State(encoded: Int) extends AnyVal {
    override def toString: String = (encoded + 'A').toChar.toString
  }

  class Tape {
    private var tape: Array[Byte] = Array.fill[Byte](5)(0)
    private var offset = tape.length / 2

    def sum: Int = tape.map(_.toInt).sum

    def read(pos: Int): Int = {
      val tapeIndex = pos + offset
      if (tapeIndex < 0 || tapeIndex >= tape.length) {
        0
      }
      else {
        tape(tapeIndex)
      }
    }
    def write(pos: Int, value: Int): Unit = {
      val tapeIndex = pos + offset
      if (tapeIndex < 0 || tapeIndex >= tape.length) {
        if (value != 0) {
          val apos = Math.abs(pos) * 2
          val newSize = (1 << (32 - java.lang.Integer.numberOfLeadingZeros(apos - 1))) + 1
          val newOffset = newSize / 2
          val newTape = new Array[Byte](newSize)
          System.arraycopy(tape, 0, newTape, newOffset - offset, tape.length)

          println(s"Reallocating tape ${tape.length} -> ${newTape.length}")

          tape = newTape
          offset = newOffset

          tape.update(pos + newOffset, value.toByte)
        }
      }
      else {
        tape.update(tapeIndex, value.toByte)
      }
    }

    override def toString: String = {
      tape.indices.map(_ - offset).map(i => f"${i}%3d").mkString(" ") + "\n" + tape.map(i => f"${i}%3d").mkString(" ")
    }
  }

  class Actions(val encoded: Array[Byte]) extends AnyVal {
    def length: Int = encoded.length
    def apply(state: State, value: Int): Action = {
      Action(encoded(Rule(state, value).encoded))
    }
    def apply(rule: Rule): Action = {
      Action(encoded(rule.encoded))
    }
  }

  object Direction {
    val Left = Direction(0)
    val Right = Direction(1)
    def apply(direction: String): Direction = {
      new Direction(direction match {
        case "left" => 0
        case "right" => 1
      })
    }
  }

  case class Direction(encoded: Int) extends AnyVal {
    def move(pos: Int): Int = if (encoded == 0) pos - 1 else pos + 1

    override def toString: String = if (encoded == 0) "Left" else "Right"
  }

  case object Action {
    def apply(write: Int, direction: String, goto: String): Action = {
      new Action((0x80 | (State(goto).encoded << 2) | (Direction(direction).encoded << 1) | write).toByte)
    }
  }
  case class Action(encoded: Byte) extends AnyVal {
    def active: Boolean = (encoded & 0x80) != 0
    def goto: State = State((encoded.toInt >>> 2) & 0x1f)
    def direction: Direction = if ((encoded & 2) == 0) Direction.Left else Direction.Right
    def write: Int = encoded.toInt & 1


    override def toString: String = s"<$write, $direction, $goto>"
  }

  case object Rule {
    def apply(state: String, value: Int): Rule = {
      Rule(State(state), value)
    }
    def apply(state: State, value: Int): Rule = {
      new Rule((state.encoded << 1) | value)
    }
  }
  case class Rule(encoded: Int) extends AnyVal {
    def state: State = State((encoded >>> 1) & 0x1f)
    def value: Int = encoded & 1

    override def toString: String = s"<$state, $value>"
  }


  val INIT = raw"Begin in state (\w+)\.".r
  val STEPS = raw"Perform a diagnostic checksum after (\d+) steps\.".r
  val STATE = raw"In state (\w+):".r
  val COND = raw"\s+If the current value is (\d+):".r
  val WRITE = raw"\s+- Write the value (\d+).".r
  val DIRECTION = raw"\s+- Move one slot to the (\w+).".r
  val GOTO = raw"\s+- Continue with state (\w+).".r


  def load(file: String): (State, Int, Actions) = {
    case class P(initial: String, steps: Int, curState: String, curCond: Int, actions: Map[(String, Int), A]) {
      def withAction(f: A => A): P = {
        val key = (curState, curCond)
        val newActions = f(actions.getOrElse(key, A.apply()))

        copy(actions = actions.updated(key, newActions))
      }
    }
    case class A(write: Int = -1, direction: String = "", goto: String = "")

    val program: P = Source.fromResource(file).getLines().foldLeft(P("", 0, "", -1, Map.empty)) { (p, line) =>
        line match {
          case INIT(s) => p.copy(initial = s)
          case STEPS(s) => p.copy(steps = s.toInt)
          case STATE(s) => p.copy(curState = s)
          case COND(s) => p.copy(curCond = s.toInt)
          case WRITE(s) => p.withAction(_.copy(write = s.toInt))
          case DIRECTION(s) => p.withAction(_.copy(direction = s))
          case GOTO(s) => p.withAction(_.copy(goto = s))
          case _ => p
        }
    }

    val actions = program.actions.map {
      case ((state, value), action) => Rule(state, value) -> Action(action.write, action.direction, action.goto)
    }

    val maxEncoded = actions.keysIterator.map(_.encoded).max

    val encodedActions = new Array[Byte](maxEncoded + 1)
    actions.foreach {
      case (rule, action) => encodedActions.update(rule.encoded, action.encoded)
    }

    val a = new Actions(encodedActions)

    (State(program.initial), program.steps, a)
  }
}

class Day25A extends FlatSpec with Matchers {
  import Day25._

  def solve(initial: State, steps: Int, actions: Actions): Int = {
    val tape = new Tape
    @tailrec
    def round(state: State, pos: Int, steps: Int): Unit = {
      if (steps != 0) {
        val value = tape.read(pos)
        val action = actions(state, value)
        tape.write(pos, action.write)
        round(action.goto, action.direction.move(pos), steps - 1)
      }
    }

    round(initial, 0, steps)
    tape.sum
  }

  "Solver" should "print result" in {
    val (initial, steps, actions) = load("Day25.txt")

    println(solve(initial, steps, actions))

  }

  it should "pass test vectors" in {
    val (initial, steps, actions) = load("Day25-test.txt")

    solve(initial, steps, actions) should be(3)
  }
}

