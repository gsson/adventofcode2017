import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.LongMap
import scala.io.Source

object Day22 {


  object Coord {
    def apply(x: Int, y: Int): Coord = {
      val yp = y.toLong << 32
      val xp = x.toLong & 0xffffffffL
      new Coord(xp | yp)
    }
  }

  sealed trait Direction {
    def left: Direction
    def right: Direction
    def reverse: Direction
  }
  case object North extends Direction {
    override def left: Direction = West
    override def right: Direction = East
    override def reverse: Direction = South
  }
  case object South extends Direction {
    override def left: Direction = East
    override def right: Direction = West
    override def reverse: Direction = North
  }
  case object East extends Direction {
    override def left: Direction = North
    override def right: Direction = South
    override def reverse: Direction = West
  }
  case object West extends Direction {
    override def left: Direction = South
    override def right: Direction = North
    override def reverse: Direction = East
  }

  case class Coord(encoded: Long) extends AnyVal {
    def x: Int = encoded.toInt
    def y: Int = (encoded >>> 32).toInt

    def dist: Int = Math.max(Math.abs(x), Math.abs(y))

    def move(direction: Direction): Coord = direction match {
      case North => Coord(x, y - 1)
      case South => Coord(x, y + 1)
      case East => Coord(x + 1, y)
      case West => Coord(x - 1, y)
    }

    override def toString: String = s"<${x}, ${y}>"
  }

  case object Nodes {
    val Clean: Byte = '.'
    val Infected: Byte = '#'
    val Weakened: Byte = 'W'
    val Flagged: Byte = 'F'
  }
  case class Nodes(totalInfected: Int, status: LongMap[Byte]) {
    import Nodes._
    def infect(coord: Coord): Nodes = apply(coord, Infected)

    def clean(coord: Coord): Nodes = apply(coord, Clean)

    def infected(coord: Coord): Boolean = {
      status.contains(coord.encoded)
    }

    def apply(coord: Coord): Byte = status.getOrElse(coord.encoded, Clean)
    def apply(coord: Coord, b: Byte): Nodes = {
      b match {
        case Clean =>
          Nodes(totalInfected, status - coord.encoded)
        case Infected =>
          Nodes(totalInfected + 1, status.updated(coord.encoded, Infected))
        case _ =>
          Nodes(totalInfected, status.updated(coord.encoded, b))
      }
    }

    override def toString: String = {
      val max = status.keys.map(v => Coord(v).dist).max
      Range(-max, max + 1).map { y =>
        Range(-max, max + 1).map { x =>
          status.getOrElse(Coord(x, y).encoded, Clean).toChar
        }.mkString(" ")
      }.mkString("\n")
    }
  }

  case class Carrier(coord: Coord, direction: Direction) {
    def left: Carrier = copy(direction = direction.left)
    def right: Carrier = copy(direction = direction.right)
    def reverse: Carrier = copy(direction = direction.reverse)
    def step: Carrier = copy(coord = coord.move(direction))
  }

  def load(file: String): Nodes = {
    val lines = Source.fromResource(file).getLines().toArray
    val offset = Math.max(lines.length, lines.maxBy(_.length).length) / 2

    val infected = lines.zipWithIndex.flatMap { case (s, r) =>
      s.zipWithIndex.collect {
        case ('#', c) => (c - offset, r - offset)
      }
    }
      .map { case (c, r) => Coord(c, r).encoded -> Nodes.Infected }

    Nodes(0, LongMap(infected :_*))
  }

}

class Day22A extends FlatSpec with Matchers {
  import Day22._
  def process(carrier: Carrier, infected: Nodes): (Carrier, Nodes) = {
    val i = infected.infected(carrier.coord)
    val c2 = if (i) carrier.right else carrier.left

    val i2 = if (!i) infected.infect(c2.coord) else infected.clean(c2.coord)

    (c2.step, i2)
  }

  def solve(n: Int, carrier: Carrier, infected: Nodes): (Carrier, Nodes) = {
    def round(n: Int, carrier: Carrier, infected: Nodes): (Carrier, Nodes) = {
      if (n == 0) {
        (carrier, infected)
      }
      else {
        val (nextCarrier, nextInfected) = process(carrier, infected)
        round(n - 1, nextCarrier, nextInfected)
      }
    }

    round(n, carrier, infected)
  }

  "Solver" should "print result" in {
    val initialInfected = load("Day22.txt")
    val initialCarrier = Carrier(Coord(0, 0), North)

    val (c2, i2) = solve(10000, initialCarrier, initialInfected)
    println(c2)
    println(i2)
    println(i2.totalInfected)

  }

  it should "pass test vectors" in {
    val initialInfected = load("Day22-test.txt")
    val initialCarrier = Carrier(Coord(0, 0), North)

    val (_, i1) = solve(70, initialCarrier, initialInfected)

    val (_, i2) = solve(10000, initialCarrier, initialInfected)

    i1.totalInfected should be(41)
    i2.totalInfected should be(5587)

  }
}


class Day22B extends FlatSpec with Matchers {
  import Day22._
  import Day22.Nodes._
  def process(carrier: Carrier, infected: Nodes): (Carrier, Nodes) = {
    val b = infected(carrier.coord)
    val c2 = b match {
      case Clean => carrier.left
      case Weakened => carrier
      case Infected => carrier.right
      case Flagged => carrier.reverse
    }
    val i2 = b match {
      case Clean => infected(c2.coord, Weakened)
      case Weakened => infected(c2.coord, Infected)
      case Infected => infected(c2.coord, Flagged)
      case Flagged => infected(c2.coord, Clean)
    }

    (c2.step, i2)
  }

  def solve(n: Int, carrier: Carrier, infected: Nodes): (Carrier, Nodes) = {
    def round(n: Int, carrier: Carrier, infected: Nodes): (Carrier, Nodes) = {
      if (n == 0) {
        (carrier, infected)
      }
      else {
        val (nextCarrier, nextInfected) = process(carrier, infected)
        round(n - 1, nextCarrier, nextInfected)
      }
    }

    round(n, carrier, infected)
  }

  "Solver" should "print result" in {
    val initialInfected = load("Day22.txt")
    val initialCarrier = Carrier(Coord(0, 0), North)

    val (c2, i2) = solve(10000000, initialCarrier, initialInfected)
    println(i2.totalInfected)
  }

  it should "pass test vectors" in {
    val initialInfected = load("Day22-test.txt")
    val initialCarrier = Carrier(Coord(0, 0), North)

    val (_, i1) = solve(100, initialCarrier, initialInfected)
    i1.totalInfected should be(26)

    val (_, i2) = solve(10000000, initialCarrier, initialInfected)
    i2.totalInfected should be(2511944)
  }
}