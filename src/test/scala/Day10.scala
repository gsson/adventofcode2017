import java.nio.charset.StandardCharsets.US_ASCII
import javax.xml.bind.DatatypeConverter

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source

object Day10 {
  object KnotState {
    def apply(n: Int): KnotState = {
      KnotState(0, 0, Vector.tabulate(n)(identity))
    }
  }
  case class KnotState(position: Int, skip: Int, numbers: Vector[Int]) {
    def twist(length: Int): KnotState = {
      if (length == 0) {
        this
      }
      else {
        val start = position % numbers.length
        val end = (position + length) % numbers.length
        if (start >= end) {
          val head = numbers.view(start, numbers.length)
          val tail = numbers.view(0, end)
          val reversed = (head ++ tail).reverse
          val reversedHead = reversed.view(0, reversed.length - end)
          val reversedTail = reversed.view(reversed.length - end, reversed.length)

          val twistedNumbers = numbers
            .patch(start, reversedHead, reversedHead.length)
            .patch(0, reversedTail, reversedTail.length)

          KnotState(position + length + skip, skip + 1, twistedNumbers)
        }
        else {
          val reversed = numbers.view(start, end).reverse
          val twistedNumbers = numbers
            .patch(start, reversed, length)
          KnotState(position + length + skip, skip + 1, twistedNumbers)
        }
      }
    }

    def check: Int = {
      numbers(0) * numbers(1)
    }
  }

  @tailrec
  def knot(numbers: KnotState, lengths: List[Int]): KnotState = {
    lengths match {
      case length :: rest =>
        val knotted_numbers = numbers.twist(length)
        knot(knotted_numbers, rest)
      case Nil => numbers
    }
  }

  @tailrec
  def knotRounds(rounds: Int, numbers: KnotState, lengths: List[Int]): KnotState = {
    if (rounds == 0) {
      numbers
    }
    else {
      val nextNumbers = knot(numbers, lengths)
      knotRounds(rounds - 1, nextNumbers, lengths)
    }
  }


}

class Day10A extends FlatSpec with Matchers {
  import Day10._
  def load(file: String): List[Int] = {
    Source.fromResource(file).getLines()
      .flatMap(_.split(','))
      .map(_.toInt)
      .toList
  }

  "Solver" should "print result" in {
    val input = load("Day10.txt")

    val result = knot(KnotState(256), input)
    println(result.check)
  }

  it should "pass test vectors" in {
    val result = knot(KnotState(5), List(3, 4, 1, 5))

    result.check should be(12)
  }
}

class Day10B extends FlatSpec with Matchers {
  import Day10._

  def load(file: String): List[Int] = {
    val input = Thread.currentThread().getContextClassLoader.getResourceAsStream(file)
    Stream.continually(input.read).takeWhile(_ > 32).toList
  }

  def solver(input: List[Int]): String = {
    val sparse = knotRounds(64, KnotState(256), input ++ List(17, 31, 73, 47, 23))
    val dense = sparse.numbers.grouped(16).map(_.reduce(_^_))

    DatatypeConverter.printHexBinary(dense.map(_.toByte).toArray).toLowerCase
  }

  "Solver" should "print result" in {
    val input = load("Day10.txt")
    println(solver(input))
  }

  it should "pass test vectors" in {
    solver(List()) should be("a2582a3a0e66e6e86e3812dcb672a272")
    solver("AoC 2017".getBytes(US_ASCII).map(_.toInt).toList) should be("33efeb34ea91902bb2f59c9920caa6cd")
    solver("1,2,3".getBytes(US_ASCII).map(_.toInt).toList) should be("3efbe78a8d82f29979031a4aa0b16a9d")
    solver("1,2,4".getBytes(US_ASCII).map(_.toInt).toList) should be("63960835bcdc130f0b66d7ff4f6a5a8e")
  }
}
