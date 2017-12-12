import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source

class Day5A extends FlatSpec with Matchers {
  def solverRec(input: Int*): Int = {
    @tailrec
    def next(pc: Int, n: Int, data: IndexedSeq[Int]): Int = {
      if (pc < 0 || pc >= data.length) {
        n
      }
      else {
        val jumpOffset = data(pc)
        next(pc + jumpOffset, n + 1, data.updated(pc, jumpOffset + 1))
      }
    }

    next(0, 0, input.toIndexedSeq)
  }


  def solver(input: Int*): Int = {
    val data = Array(input: _*)

    var pc = 0
    var n = 0

    while (pc >= 0 && pc < data.length) {
      val jumpOffset = data(pc)
      data.update(pc, jumpOffset + 1)
      pc = pc + jumpOffset
      n = n + 1
    }
    n
  }

  "Solver" should "print result" in {
    val input = Source.fromResource("Day5.txt").getLines()
      .map(_.toInt)
      .toIndexedSeq
    println(solver(input :_*))
  }

  it should "pass test vectors" in {
    solver(0, 3, 0, 1, -3) should be(5)
  }

  "Recursive solver" should "print result" in {
    val input = Source.fromResource("Day5.txt").getLines()
      .map(_.toInt)
      .toIndexedSeq
    println(solverRec(input :_*))
  }

  it should "pass test vectors" in {
    solverRec(0, 3, 0, 1, -3) should be(5)
  }
}

class Day5B extends FlatSpec with Matchers {
  def solverRec(input: Vector[Int]): Int = {
    def nextJumpOffset(jumpOffset: Int) = {
      if (jumpOffset >= 3)
        jumpOffset - 1
      else
        jumpOffset + 1
    }

    @tailrec
    def next(pc: Int, n: Int, data: Vector[Int]): Int = {
      if (pc < 0 || pc >= data.length) {
        n
      }
      else {
        val jumpOffset = data(pc)
        next(pc + jumpOffset, n + 1, data.updated(pc, nextJumpOffset(jumpOffset)))
      }
    }

    next(0, 0, input)
  }

  def solver(input: Int*): Int = {
    val data = Array(input: _*)

    var pc = 0
    var n = 0

    while (pc >= 0 && pc < data.length) {
      val jumpOffset = data(pc)
      if (jumpOffset >= 3)
        data.update(pc, jumpOffset - 1)
      else
        data.update(pc, jumpOffset + 1)
      pc = pc + jumpOffset
      n = n + 1
    }
    n
  }

  "Solver" should "print result" in {
    val input = Source.fromResource("Day5.txt").getLines()
      .map(_.toInt)
      .toSeq
    println(solver(input: _*))
  }

  it should "pass test vectors" in {
    solver(0, 3, 0, 1, -3) should be(10)
  }

  "Recursive solver" should "print result" in {
    val input = Source.fromResource("Day5.txt").getLines()
      .map(_.toInt)
      .toSeq
    println(solverRec(Vector(input: _*)))
  }

  it should "pass test vectors" in {
    solverRec(Vector(0, 3, 0, 1, -3)) should be(10)
  }
}
