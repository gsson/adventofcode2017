import java.nio.ByteBuffer

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

object Day14 {

  case object BitGrid {
    val GridSide: Int = 128
    val GridLongs: Int = GridSide * GridSide / java.lang.Long.SIZE
    def apply(): BitGrid = BitGrid(Array.fill(GridLongs)(0))
    def mask(index: Int): Long = 0x8000000000000000L >>> index
    def bitAt(bits: Long, index: Int): Boolean = {
      (bits & mask(index)) != 0
    }
  }

  case class BitGrid(bits: Array[Long]) extends AnyVal {
    import BitGrid._
    def row(row: Int): Iterator[Boolean] = {
      val i = row << 1
      val f = bits(i)
      val l = bits(i + 1)
      Iterator.range(0, 64).map(n => (f & mask(n)) != 0) ++
      Iterator.range(0, 64).map(n => (l & mask(n)) != 0)
    }

    def apply(row: Int, col: Int): Boolean = {
      val i = row << 1
      if (col < 64) {
        bitAt(bits(i), col)
      }
      else {
        bitAt(bits(i + 1), col - 64)
      }
    }

    def setRow(row: Int, firstBits: Long, lastBits: Long) = {
      val i = row << 1
      bits.update(i, firstBits)
      bits.update(i + 1, lastBits)
    }

    def set(row: Int, col: Int): Unit = {
      val i = row << 1
      if (col < 64) {
        bits.update(i, bits(i) | mask(col))
      }
      else {
        bits.update(i + 1, bits(i + 1) | mask(col))
      }
    }

    def bitCount: Int = {
      bits.foldLeft(0) {
        (a, b) => a + java.lang.Long.bitCount(b)
      }
    }

    def inverted: BitGrid = {
      val invertedBits = bits.clone()
      invertedBits.indices.foreach(i => invertedBits.update(i, ~invertedBits(i)))
      BitGrid(invertedBits)
    }
  }

  case object DiskMap {
    val LengthTrailer = Array(17, 31, 73, 47, 23)
    def apply(): DiskMap = DiskMap(BitGrid())
  }

  case class DiskMap(bits: BitGrid) {
    import DiskMap._
    private val state = Day10Mutable.MutableKnotState(256)
    def groups: Int = {
      val visited = bits.inverted

      def shouldVisit(row: Int, col: Int): Boolean = !visited(row, col) && bits(row, col)

      def visit(row: Int, col: Int): Unit = {
        visited.set(row, col)
        if (col > 0 && shouldVisit(row, col - 1)) {
          visit(row, col - 1)
        }
        if (col < BitGrid.GridSide - 1 && shouldVisit(row, col + 1)) {
          visit(row, col + 1)
        }
        if (row > 0 && shouldVisit(row - 1, col)) {
          visit(row - 1, col)
        }
        if (row < BitGrid.GridSide - 1 && shouldVisit(row + 1, col)) {
          visit(row + 1, col)
        }
      }

      @tailrec
      def scanCol(count: Int, row: Int, col: Int): Int = {
        if (col == BitGrid.GridSide)
          count
        else {
          val nextCount = if (shouldVisit(row, col)) {
            visit(row, col)
            count + 1
          }
          else {
            count
          }

          scanCol(nextCount, row, col + 1)
        }
      }

      @tailrec
      def scanRow(count: Int, row: Int): Int = {
        if (row == BitGrid.GridSide)
          count
        else {
          scanRow(scanCol(count, row, 0), row + 1)
        }
      }

      scanRow(0, 0)
    }

    def toLenghts(input: String): Array[Int] = {
      var i = 0
      val result = Array.fill(input.length + LengthTrailer.length)(0)
      while (i < input.length) {
        result.update(i, input.charAt(i))
        i += 1
      }
      System.arraycopy(LengthTrailer, 0, result, input.length, LengthTrailer.length)
      result
    }

    def knot(row: Int, input: String): Unit = {
      val lengths = toLenghts(input)
      state.reset
      Day10Mutable.knotRounds(64, state, lengths)

      val bb = ByteBuffer.wrap(state.denseHash)
      bits.setRow(row, bb.getLong(), bb.getLong())
    }

    def count: Int = {
      bits.bitCount
    }

    def rowString(row: Int): String = {
      bits.row(row).map(if (_) "#" else ".").mkString("")
    }

    override def toString: String = {
      Range(0, 128)
        .map(rowString).mkString("\n")
    }

  }
}

class Day14A extends FlatSpec with Matchers {
  import Day14._

  def solver(input: String): Int = {
    val grid = DiskMap()
    Range(0, 128)
      .foreach(n => grid.knot(n, s"$input-$n"))

    grid.count
  }

  "Solver" should "print result" in {
    println(solver("hwlqcszp"))
  }

  it should "pass test vectors" in {
    solver("flqrgnkx") should be(8108)
  }
}

class Day14B extends FlatSpec with Matchers {
  import Day14._

  def solver(input: String): Int = {
    val grid = DiskMap()
    Range(0, 128)
      .foreach(n => grid.knot(n, s"$input-$n"))

    grid.groups
  }


  "Solver" should "print result" in {
    println(solver("hwlqcszp"))
  }

  it should "pass test vectors" in {
    solver("flqrgnkx") should be(1242)
  }
}
