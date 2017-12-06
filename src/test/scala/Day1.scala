package adventofcode2017

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day1A extends FlatSpec with Matchers {
  def digit(c: Char) = Character.digit(c, 10)

  def solver(input: String): Int = {
    input.foldLeft((0, input.last)) {
      case ((n, prev), next) if prev == next => (n + digit(next), next)
      case ((n, _), next) => (n, next)
    }._1
  }

  "Solver" should "print result" in {
    val input = Source.fromResource("Day1.txt").mkString

    println(solver(input))
  }

  it should "pass test vectors" in {
    solver("1122") should be(3)

    solver("1111") should be(4)

    solver("1234") should be(0)

    solver("91212129") should be(9)
  }
}

class Day1B extends FlatSpec with Matchers {
  def digit(c: Char) = Character.digit(c, 10)

  def solver(input: String): Int = {
    val rotatedInput = input.slice(input.length / 2, input.length) + input.slice(0, input.length / 2)
    val zipped = input.zip(rotatedInput)
    zipped.foldLeft(0) {
      case (n, (a, b)) if a == b => n + digit(a)
      case (n, _) => n
    }
  }

  "Solver" should "print result" in {
    val input = Source.fromResource("Day1.txt").mkString

    println(solver(input))
  }

  it should "pass test vectors" in {
    solver("1212") should be(6)

    solver("1221") should be(0)

    solver("123425") should be(4)

    solver("123123") should be(12)

    solver("12131415") should be(4)
  }
}

