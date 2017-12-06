import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day2A extends FlatSpec with Matchers {
  def digit(c: Char) = Character.digit(c, 10)

  def rowsum(row: String): Int = {
    val digits = row.split("\\s+").map(_.toInt)
    digits.max - digits.min
  }

  def solver(input: String*): Int = {
    input.map(rowsum).sum
  }

  "Solver" should "print result" in {
    val input = Source.fromResource("Day2.txt")
      .getLines()
      .toSeq

    println(solver(input: _*))
  }

  it should "pass test vectors" in {
    solver(
      "5 1 9 5",
      "7 5 3",
      "2 4 6 8"
    ) should be(18)
  }
}

class Day2B extends FlatSpec with Matchers {
  def digit(c: Char) = Character.digit(c, 10)

  def rowsum(row: String): Int = {
    val digits = row.split("\\s+").map(_.toInt)
    digits.combinations(2)
      .map(_.sorted)
      .map(a => (a(0), a(1)))
      .collect {
        case (smaller, larger) if larger % smaller == 0 => larger / smaller
      }.next()
  }

  def solver(input: String*): Int = {
    input.map(rowsum).sum
  }

  "Solver" should "print result" in {
    val input = Source.fromResource("Day2.txt")
      .getLines()
      .toSeq

    println(solver(input: _*))
  }

  it should "pass test vectors" in {
    solver(
      "5 9 2 8",
      "9 4 7 3",
      "3 8 6 5"
    ) should be(9)
  }
}
