import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

object Day21 {
  def bit(c: Char): Short = if (c == '.') 0 else 1
  def row(s: String): Short = s.length match {
    case 2 =>
      (bit(s(0)) << 0 | bit(s(1)) << 1).toShort
    case 3 =>
      (bit(s(0)) << 0 | bit(s(1)) << 1 | bit(s(2)) << 2).toShort
    case 4 =>
      (bit(s(0)) << 0 | bit(s(1)) << 1 | bit(s(2)) << 2 | bit(s(3)) << 3).toShort
  }

  def encode(s: String): Short = {
    val lines = s.split('/')
    lines.length match {
      case 2 =>
        ((row(lines(0)) << 0) | (row(lines(1)) << 4)).toShort
      case 3 =>
        ((row(lines(0)) << 0) | (row(lines(1)) << 4) | (row(lines(2)) << 8)).toShort
      case 4 =>
        ((row(lines(0)) << 0) | (row(lines(1)) << 4) | (row(lines(2)) << 8) | (row(lines(3)) << 12)).toShort
    }
  }

  def toArray(s: String): Array[Byte] = {
    def toByte(c: Char): Byte = if (c == '.') 0 else 1

    val lines = s.split('/')
    lines.length match {
      case 2 =>
        Array(
          toByte(lines(0)(0)), toByte(lines(0)(1)),
          toByte(lines(1)(0)), toByte(lines(1)(1))
        )
      case 3 =>
        Array(
          toByte(lines(0)(0)), toByte(lines(0)(1)), toByte(lines(0)(2)),
          toByte(lines(1)(0)), toByte(lines(1)(1)), toByte(lines(1)(2)),
          toByte(lines(2)(0)), toByte(lines(2)(1)), toByte(lines(2)(2))
        )
      case 4 =>
        Array(
          toByte(lines(0)(0)), toByte(lines(0)(1)), toByte(lines(0)(2)), toByte(lines(0)(3)),
          toByte(lines(1)(0)), toByte(lines(1)(1)), toByte(lines(1)(2)), toByte(lines(1)(3)),
          toByte(lines(2)(0)), toByte(lines(2)(1)), toByte(lines(2)(2)), toByte(lines(2)(3)),
          toByte(lines(3)(0)), toByte(lines(3)(1)), toByte(lines(3)(2)), toByte(lines(3)(3))
        )
    }
  }

  def coordf(side: Int): ((Int, Int)) => Int = {
    case (c, r) => c + r * (side + 1)
  }

  def identf(side: Int): IndexedSeq[Int] = {
    map(side, identity[(Int, Int)])
  }

  def rotf(side: Int): IndexedSeq[Int] = {
    map(side, { case (col, row) => (side - row - 1, col)})
  }

  def flipvf(side: Int): IndexedSeq[Int] = {
    map(side, { case (col, row) => (col, side - row - 1)})
  }

  def fliphf(side: Int): IndexedSeq[Int] = {
    map(side, { case (col, row) => (side - col - 1, row)})
  }

  def combine(a: IndexedSeq[Int], b: IndexedSeq[Int]): IndexedSeq[Int] = a.map(b)

  def map(side: Int, f: ((Int, Int)) => (Int, Int)): IndexedSeq[Int] = {
    val coord = coordf(side)

    def cols(row: Int): IndexedSeq[Int] = {
      Range(0, side).map {
        col => coord(f(col, row))
      }
    }

    Range(0, side - 1).flatMap { row =>
      cols(row) :+ coord(side, row)
    } ++ cols(side - 1)
  }

  def encodeVariants(from: String, to: String): Map[Short, Short] = {
    val transforms = from.length match {
      case 5 => Transform.T2
      case 11 => Transform.T3
      case 19 => Transform.T4
    }
    val encodedTo = encode(to)
    transforms
      .map(_.apply(from))
      .map(f => encode(f) -> encodedTo)
      .toMap
  }

  case class Transform(t: IndexedSeq[Int]) extends AnyVal {
    def apply(s: String): String = t.map(s).mkString("")
  }

  object Transform {
    val T2 = transforms(2)
    val T3 = transforms(3)
    val T4 = transforms(4)

    def transforms(side: Int): Set[Transform] = {
      val id = identf(side)
      val rot1 = rotf(side)
      val rot2 = rot1.map(rot1)
      val rot3 = rot2.map(rot1)
      val fh = fliphf(side)
      val fv = flipvf(side)
      val fvh = fh.map(fv)

      val variants0 = Set(id, rot1, rot2, rot3)
      val variants1 = Set(id, rot1, rot2, rot3).map(_.map(fv))
      val variants2 = Set(id, rot1, rot2, rot3).map(_.map(fh))
      val variants3 = Set(id, rot1, rot2, rot3).map(_.map(fvh))

      (variants0 | variants1 | variants2 | variants3).map(new Transform(_))
    }
  }


  object Grid {

    def windowOffsets(size: Int, side: Int): IndexedSeq[Int] = {
      val wrows = Range(0, side * side, side * size)
      val wcols = Range(0, side, size)
      wrows.flatMap(rowOffset => wcols.map(rowOffset + _))
    }

    def apply() = {
      new Grid(3, toArray(".#./..#/###"))
    }
  }

  class Grid(side: Int, bits: Array[Byte]) {
    def sum: Int = bits.map(_.toInt).sum

    def bit(offset: Int): Int = if (bits(offset) != 0) 1 else 0
    def win3(offset: Int): Short = {
      val row0 = (bit(offset + 2) << 2) | (bit(offset + 1) << 1) | bit(offset + 0)
      val row1 = (bit(offset + side + 2) << 6) | (bit(offset + side + 1) << 5) | (bit(offset + side + 0) << 4)
      val row2 = (bit(offset + 2 * side + 2) << 10) | (bit(offset + 2 * side + 1) << 9) | (bit(offset + 2 * side + 0) << 8)
      (row0 | row1 | row2).toShort
    }
    def win2(offset: Int): Short = {
      val row0 = (bit(offset + 1) << 1) | bit(offset + 0)
      val row1 = (bit(offset + side + 1) << 5) | (bit(offset + side + 0) << 4)
      (row0 | row1).toShort
    }

    def encodeWindows(window: Int): IndexedSeq[Short] = {
      val win = if (window == 2) win2 _ else win3 _
      Grid.windowOffsets(window, side).map(win)
    }

    def bitSet(in: Short, o: Int): Byte = if ((in & o) != 0) 1 else 0


    def decode3(in: Short, offset: Int, side: Int, out: Array[Byte]): Unit = {
      out.update(offset + 0, bitSet(in, 1 << 0))
      out.update(offset + 1, bitSet(in, 1 << 1))
      out.update(offset + 2, bitSet(in, 1 << 2))

      out.update(offset + side + 0, bitSet(in, 1 << 4))
      out.update(offset + side + 1, bitSet(in, 1 << 5))
      out.update(offset + side + 2, bitSet(in, 1 << 6))

      out.update(offset + 2 * side + 0, bitSet(in, 1 << 8))
      out.update(offset + 2 * side + 1, bitSet(in, 1 << 9))
      out.update(offset + 2 * side + 2, bitSet(in, 1 << 10))
    }

    def decode4(in: Short, offset: Int, side: Int, out: Array[Byte]): Unit = {
      out.update(offset + 0, bitSet(in, 1 << 0))
      out.update(offset + 1, bitSet(in, 1 << 1))
      out.update(offset + 2, bitSet(in, 1 << 2))
      out.update(offset + 3, bitSet(in, 1 << 3))

      out.update(offset + side + 0, bitSet(in, 1 << 4))
      out.update(offset + side + 1, bitSet(in, 1 << 5))
      out.update(offset + side + 2, bitSet(in, 1 << 6))
      out.update(offset + side + 3, bitSet(in, 1 << 7))

      out.update(offset + 2 * side + 0, bitSet(in, 1 << 8))
      out.update(offset + 2 * side + 1, bitSet(in, 1 << 9))
      out.update(offset + 2 * side + 2, bitSet(in, 1 << 10))
      out.update(offset + 2 * side + 3, bitSet(in, 1 << 11))

      out.update(offset + 3 * side + 0, bitSet(in, 1 << 12))
      out.update(offset + 3 * side + 1, bitSet(in, 1 << 13))
      out.update(offset + 3 * side + 2, bitSet(in, 1 << 14))
      out.update(offset + 3 * side + 3, bitSet(in, 1 << 15))
    }

    def decode(size: Int, side: Int, encoded: IndexedSeq[Short]): Array[Byte] = {
      val out = new Array[Byte](side * side)
      if (size == 3) {
        Grid.windowOffsets(3, side)
          .zip(encoded.indices)
          .foreach { case (offset, window) =>
            decode3(encoded(window), offset, side, out)
          }
      }
      else {
        Grid.windowOffsets(4, side)
          .zip(encoded.indices)
          .foreach { case (offset, window) =>
            decode4(encoded(window), offset, side, out)
          }
      }
      out
    }

    def apply(rules2: Map[Short, Short], rules3: Map[Short, Short]): Grid = {

      if (side % 2 == 0) {
        val newSide = side * 3 / 2
        val transformed = encodeWindows(2).map(rules2)
        new Grid(newSide, decode(3, newSide, transformed))
      }
      else {
        val newSide = side * 4 / 3
        val transformed = encodeWindows(3).map(rules3)
        new Grid(newSide, decode(2, newSide, transformed))
      }
    }

    override def toString: String = {
      Range(0, side * side, side).map { rowOffset =>
        Range(0, side).map { col =>
          if (bits(rowOffset + col) == 0) '.' else '#'
        }.mkString("")
      }.mkString("/")
    }
  }

  val P = raw"(\S+) => (\S+)".r

  def load(file: String): (Map[Short, Short], Map[Short, Short]) = {
    Source.fromResource(file).getLines().foldLeft((Map.empty[Short, Short], Map.empty[Short, Short])) { (rules, line) =>
      line match {
        case P(from, to) if from.length == 5 =>
          (rules._1 ++ encodeVariants(from, to), rules._2)
        case P(from, to) if from.length == 11 =>
          (rules._1, rules._2 ++ encodeVariants(from, to))
      }
    }
  }
}

class Day21A extends FlatSpec with Matchers {

  import Day21._

  def solver(rules2: Map[Short, Short], rules3: Map[Short, Short], rounds: Int): Grid = {
    def round(n: Int, grid: Grid): Grid = {
      if (n == 0)
        grid
      else {
        round(n - 1, grid(rules2, rules3))
      }
    }

    round(rounds, Grid())
  }

  "Solver" should "print result" in {
    val (rules2, rules3) = load("Day21.txt")

    val solution = solver(rules2, rules3, 18)
    println(solution.sum)

  }

  it should "pass test vectors" in {
    val (rules2, rules3) = load("Day21-test.txt")

    solver(rules2, rules3, 2).sum should be(12)
  }


}