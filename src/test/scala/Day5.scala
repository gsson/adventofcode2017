import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day5A extends FlatSpec with Matchers {
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
      .toSeq
    println(solver(input: _*))
  }

  it should "pass test vectors" in {
    solver(0, 3, 0, 1, -3) should be(5)
  }
}

class Day5B extends FlatSpec with Matchers {
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
}
