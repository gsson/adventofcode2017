import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source

object Day16 {
  object Line {
    def apply(): Line = {
      new ImmutableLine(Array[Byte]('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p'))
    }
    def mutable: Line = {
      new MutableLine(Array[Byte]('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p'))
    }
  }
  trait Line {
    def spin(x: Int): Line
    def exchange(a: Int, b: Int): Line
    def partner(a: Byte, b: Byte): Line
    def dancers: Array[Byte]
  }

  case class ImmutableLine(dancers: Array[Byte]) extends Line {
    def spin(x: Int): Line = {
      val nextDancers = new Array[Byte](dancers.length)
      System.arraycopy(dancers, dancers.length - x, nextDancers, 0, x)
      System.arraycopy(dancers, 0, nextDancers, x, dancers.length - x)
      ImmutableLine(nextDancers)
    }

    def exchange(a: Int, b: Int): Line = {
      val nextDancers = dancers.clone()
      nextDancers.update(a, dancers(b))
      nextDancers.update(b, dancers(a))
      ImmutableLine(nextDancers)
    }

    def partner(a: Byte, b: Byte): Line = {
      exchange(dancers.indexOf(a), dancers.indexOf(b))
    }

    override def toString = {
      dancers.map(_.toChar).mkString("")
    }
  }

  final case class ByteArrayExt(ba: Array[Byte]) extends AnyVal {
    def linearSearch(a: Byte, b: Byte): (Int, Int) = {
      linearSearch(0, a, b)
    }

    def linearSearch(start: Int, a: Byte, b: Byte): (Int, Int) = {
      val (firstIndex, remaining) = findFirst(start, a, b)
      (firstIndex, findSecond(firstIndex + 1, remaining))
    }

    def findFirst(start: Int, a: Byte, b: Byte): (Int, Byte) = {
      val l = ba.length
      var i = start
      while (i < l) {
        val n = ba(i)
        if (n == a) {
          return (i, b)
        }
        if (n == b) {
          return (i, a)
        }
        i += 1
      }
      throw new IllegalStateException()
    }

    def findSecond(start: Int, a: Byte): Int = {
      val l = ba.length
      var i = start
      while (i < l) {
        if (ba(i) == a) {
          return i
        }
        i += 1
      }
      throw new IllegalStateException()
    }
  }

  implicit def byteArrayExt(ba: Array[Byte]): ByteArrayExt = ByteArrayExt(ba)

  case class MutableLine(dancers: Array[Byte]) extends Line {
    private var offset = 0
    def spin(x: Int): Line = {
      offset = offset + (dancers.length - x)
      this
    }

    def exchange(a: Int, b: Int): Line = {
      val aa = (a + offset) % dancers.length
      val ab = (b + offset) % dancers.length
      val t = dancers(aa)
      dancers.update(aa, dancers(ab))
      dancers.update(ab, t)
      this
    }


    def partner(na: Byte, nb: Byte): Line = {
      val (aa, ab) = dancers.linearSearch(na, nb)
      val t = dancers(aa)
      dancers.update(aa, dancers(ab))
      dancers.update(ab, t)
      this
    }

    override def toString = {
      (Range(offset % dancers.length, dancers.length) ++ Range(0, offset % dancers.length))
        .map(dancers)
        .map(_.toChar)
        .mkString("")
    }
  }

  sealed trait Move
  case class Spin(x: Int) extends Move
  case class Exchange(a: Int, b: Int) extends Move
  case class Partner(a: Byte, b: Byte) extends Move

  def interpret(line: Line, move: Move): Line = {
    move match {
      case Spin(x) => line.spin(x)
      case Exchange(a, b) => line.exchange(a, b)
      case Partner(a, b) => line.partner(a, b)
    }
  }

  def interpret(line: Line, moves: Seq[Move]): Line = {
    moves.foldLeft(line)  {
      case (l, move) => interpret(l, move)
    }
  }

  def interpretMutable(line: MutableLine, moves: Seq[Move]): MutableLine = {
    moves.foreach(interpret(line, _))
    line
  }

  val S = raw"s(\d+)"r
  val X = raw"x(\d+)/(\d+)"r
  val P = raw"p([a-p])/([a-p])"r

  def load(file: String): Seq[Move] = {
    Source.fromResource(file).getLines()
      .flatMap(_.split(','))
      .map {
        case S(n) => Spin(n.toInt)
        case X(a, b) => Exchange(a.toInt, b.toInt)
        case P(a, b) => Partner(a(0).toByte, b(0).toByte)
      }
      .toList
  }
}

class Day16A extends FlatSpec with Matchers {
  import Day16._

  "Immutable solver" should "print result" in {
    val result = load("Day16.txt").foldLeft(Line()) {
      case (line, move) => interpret(line, move)
    }

    println(result)
  }

  "Mutable solver" should "print result" in {
    val result = load("Day16.txt").foldLeft(Line.mutable) {
      case (line, move) => interpret(line, move)
    }

    println(result)
  }

  it should "pass test vectors" in {
    val steps = Seq(Spin(1), Exchange(3, 4), Partner('e', 'b'))
    interpret(new ImmutableLine(Array('a', 'b', 'c', 'd', 'e')), steps).toString should be("baedc")
    interpretMutable(new MutableLine(Array('a', 'b', 'c', 'd', 'e')), steps).toString should be("baedc")
  }
}

class Day16B extends FlatSpec with Matchers {
  import Day16._

  "Immutable solution" should "pass test vectors" in {
    val steps = load("Day16.txt")
    val initial = Array[Byte]('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p')

    def findPeriod(initial: Array[Byte]): Int = {
      Iterator.iterate(interpret(Line(), steps))(interpret(_, steps))
        .zipWithIndex
        .dropWhile(v => !initial.sameElements(v._1.dancers))
        .next()._2 + 1
    }

    val period = findPeriod(initial)

    val rounds = 1000000000 % period
    val line = Iterator.iterate(Line())(interpret(_, steps))
      .drop(rounds)
      .next()
    println(line)
  }

  "Mutable solution" should "pass test vectors" in {
    val steps = load("Day16.txt")
    val initial = Array[Byte]('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p')


    def findPeriod(initial: Array[Byte]): Int = {
      val line = new MutableLine(initial.clone())
      Iterator.from(1).dropWhile { _ =>
        interpretMutable(line, steps)
        !java.util.Arrays.equals(initial, line.dancers)
      }.next()
    }

    val period = findPeriod(initial)
    val line = new MutableLine(initial.clone())

    val rounds = 1000000000 % period
    @tailrec
    def round(count: Int, line: MutableLine): Line = {
      if (count == 0) {
        line
      }
      else {
        round(count - 1, interpretMutable(line, steps))
      }
    }

    println(round(rounds, line))
  }
}

