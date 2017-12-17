import java.nio.charset.StandardCharsets.US_ASCII
import javax.xml.bind.DatatypeConverter

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source

object Day10 {
  object KnotState {
    def apply(n: Int): KnotState = {
      KnotState(0, 0, Array.tabulate(n)(identity))
    }
  }

  case class KnotState(position: Int, skip: Int, numbers: Array[Int]) {
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

    def check: Int = numbers(0) * numbers(1)

    def denseHash: Array[Byte] = numbers.grouped(16).map(_.reduce(_^_)).map(_.toByte).toArray
  }

  @tailrec
  def knot(numbers: KnotState, lengths: Seq[Int]): KnotState = {
    lengths match {
      case length :: rest =>
        val knotted_numbers = numbers.twist(length)
        knot(knotted_numbers, rest)
      case Nil => numbers
    }
  }

  @tailrec
  def knotRounds(rounds: Int, numbers: KnotState, lengths: Seq[Int]): KnotState = {
    if (rounds == 0) {
      numbers
    }
    else {
      val nextNumbers = knot(numbers, lengths)
      knotRounds(rounds - 1, nextNumbers, lengths)
    }
  }
}

object Day10Mutable {
  object MutableKnotState {
    def apply(n: Int): MutableKnotState = {
      val a = new Array[Int](n)
      new MutableKnotState(a).reset
    }
  }

  class MutableKnotState(val numbers: Array[Int]) {
    private var position = 0
    private var skip = 0

    def reset: MutableKnotState = {
      position = 0
      skip = 0
      var i = 0
      while (i < numbers.length) {
        numbers.update(i, i)
        i += 1
      }
      this
    }

    def reverse(start: Int, length: Int): Unit = {
      val halfLength = length >>> 1
      val reverseStart = start + length - 1

      var i = 0
      while (i < halfLength) {
        val forwardIndex = (start + i) % numbers.length
        val reverseIndex = (reverseStart - i) % numbers.length
        val t = numbers(forwardIndex)
        numbers.update(forwardIndex, numbers(reverseIndex))
        numbers.update(reverseIndex, t)
        i += 1
      }
    }

    def twist(length: Int): MutableKnotState = {
      if (length != 0) {
        reverse(position, length)
        position += length + skip
        skip += 1
      }
      this
    }

    def check: Int = numbers(0) * numbers(1)

    def denseHash: Array[Byte] = {
      var i = 0
      var j = 0

      val result = new Array[Byte](16)
      while (i < 16) {
        j = i << 4
        var h = 0
        while (j < (i << 4) + 16) {
          h ^= numbers(j)
          j += 1
        }
        result.update(i, h.toByte)
        i += 1
      }
      result
    }
  }

  def knotRound(numbers: MutableKnotState, lengths: Array[Int]) : Unit = {
    var i = 0

    while (i < lengths.length) {
      numbers.twist(lengths(i))
      i += 1
    }
  }

  def knotRounds(rounds: Int, numbers: MutableKnotState, lengths: Array[Int]): MutableKnotState = {
    var i = 0

    while (i < rounds) {
      knotRound(numbers, lengths)

      i += 1
    }

    numbers
  }
}



class Day10A extends FlatSpec with Matchers {
  import Day10._
  def load(file: String): Seq[Int] = {
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

  def load(file: String): Seq[Int] = {
    val input = Thread.currentThread().getContextClassLoader.getResourceAsStream(file)
    Iterator.continually(input.read).takeWhile(_ > 32).toArray
  }

  def solver(input: Seq[Int]): String = {
    val state = knotRounds(64, KnotState(256), (input ++ Array(17, 31, 73, 47, 23)).toList)

    DatatypeConverter.printHexBinary(state.denseHash).toLowerCase
  }

  def mutableSolver(input: Seq[Int]): String = {
    val state = Day10Mutable.knotRounds(64, Day10Mutable.MutableKnotState(256), (input ++ Array(17, 31, 73, 47, 23)).toArray)

    DatatypeConverter.printHexBinary(state.denseHash).toLowerCase
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

  "MutableSolver" should "print result" in {
    val input = load("Day10.txt")
    println(mutableSolver(input))
  }

  it should "pass test vectors" in {
    mutableSolver(List()) should be("a2582a3a0e66e6e86e3812dcb672a272")
    mutableSolver("AoC 2017".getBytes(US_ASCII).map(_.toInt).toList) should be("33efeb34ea91902bb2f59c9920caa6cd")
    mutableSolver("1,2,3".getBytes(US_ASCII).map(_.toInt).toList) should be("3efbe78a8d82f29979031a4aa0b16a9d")
    mutableSolver("1,2,4".getBytes(US_ASCII).map(_.toInt).toList) should be("63960835bcdc130f0b66d7ff4f6a5a8e")
  }

  it should "twist" in {
    Day10Mutable.MutableKnotState(10).numbers should be(KnotState(10).numbers)
    Day10Mutable.MutableKnotState(10).twist(5).numbers should be(KnotState(10).twist(5).numbers)
    Day10Mutable.MutableKnotState(10).twist(5).twist(8).numbers should be(KnotState(10).twist(5).twist(8).numbers)

  }

}
