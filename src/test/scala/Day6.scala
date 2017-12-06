import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source

case class Banks(configuration: IndexedSeq[Int]) {
  @tailrec
  final def rebalance(count: Int, index: Int, blocks: IndexedSeq[Int]): IndexedSeq[Int] = {
    if (count == 0) {
      blocks
    }
    else {
      rebalance(count - 1, (index + 1) % blocks.length, blocks.updated(index, blocks(index) + 1))
    }
  }

  def rebalance(): Banks = {
    val (count, index) = configuration
      .zipWithIndex
      .maxBy(_._1)

    val startBlocks = configuration.updated(index, 0)
    val rebalanced = rebalance(count, (index + 1) % startBlocks.length, startBlocks)
    Banks(rebalanced)
  }
}

class Day6A extends FlatSpec with Matchers {

  @tailrec
  final def loopRebalance(history: Set[Banks], banks: Banks): Int = {
    val updated = history + banks
    if (updated.size == history.size) {
      history.size
    }
    else {
      loopRebalance(updated, banks.rebalance())
    }
  }

  def solver(input: Int*): Int = {
    val initial = Banks(input.toIndexedSeq)
    loopRebalance(Set(initial), initial.rebalance())
  }

  "Solver" should "print result" in {
    val input = Source.fromResource("Day6.txt").getLines()
      .flatMap(_.split("\\s+"))
      .map(_.toInt)
      .toSeq

    println(solver(input: _*))
  }

  it should "pass test vectors" in {
    solver(0, 2, 7, 0) should be(5)
  }
}

class Day6B extends FlatSpec with Matchers {
  @tailrec
  final def loopRebalance(history: Set[Banks], banks: Banks): Int = {
    val updated = history + banks
    if (updated.size == history.size) {
      history.size
    }
    else {
      loopRebalance(updated, banks.rebalance())
    }
  }

  @tailrec
  final def findStart(history: Set[Banks], banks: Banks): Banks = {
    val updated = history + banks
    if (updated.size == history.size) {
      banks
    }
    else {
      findStart(updated, banks.rebalance())
    }
  }

  def solver(input: Int*): Int = {
    val initial = Banks(input.toIndexedSeq)
    val start = findStart(Set(initial), initial.rebalance())
    loopRebalance(Set(start), start.rebalance())
  }

  "Solver" should "print result" in {
    val input = Source.fromResource("Day6.txt").getLines()
      .flatMap(_.split("\\s+"))
      .map(_.toInt)
      .toSeq

    println(solver(input: _*))

  }

  it should "pass test vectors" in {
    solver(0, 2, 7, 0) should be(4)
  }
}
