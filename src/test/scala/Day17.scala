import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source

object Day17 {
  def pivot(in: Array[Int]): Array[Int] = {
    val out = new Array[Int](in.length)
    in.indices.foreach( i =>
      out.update(in(i), i)
    )
    out
  }

  def spin(spin: Int): Array[Int] = {
    val values = Array.fill(2018)(Int.MaxValue)
    values.update(0, 0)
    var index = 0
    Range(1, 2018).foreach { i =>
      index = ((index + spin) % i) + 1
      values.update(i, index)
      Range(0, i).foreach { ii =>
        val n = values(ii)
        if (n >= index)
          values.update(ii, n + 1)
      }
    }
    pivot(values)
  }

  def findPosition1(spin: Int, count: Int): Int = {
    @tailrec
    def round(index: Int, valueAtIndex1: Int, value: Int): Int = {
      if (value == count) {
        valueAtIndex1
      }
      else {
        val nextIndex = ((index + spin) % value) + 1
        val nextValueAtIndex1 = if (nextIndex == 1) value else valueAtIndex1
        round(nextIndex, nextValueAtIndex1, value + 1)
      }
    }

    round(1, 1, 1)
  }
}

class Day17A extends FlatSpec with Matchers {
  import Day17._

  "Solver" should "print result" in {
    val values = spin(355)
    println(values(values.indexOf(2017) + 1))
  }
}

class Day17B extends FlatSpec with Matchers {
  import Day17._

  "Solver" should "print result" in {
    println(findPosition1(355, 50000000))
  }
}

