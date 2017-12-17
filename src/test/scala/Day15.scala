import java.nio.ByteBuffer
import java.util.stream.LongStream

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

object Day15 {

  def genA(start: Long): Iterator[Long] = Iterator.iterate(start)(n => (n * 16807) % 2147483647).drop(1)
  def genB(start: Long): Iterator[Long] = Iterator.iterate(start)(n => (n * 48271) % 2147483647).drop(1)

  def streamA(start: Long): LongStream = LongStream.iterate(start, n => (n * 16807) % 2147483647).skip(1)
  def streamB(start: Long): LongStream = LongStream.iterate(start, n => (n * 48271) % 2147483647).skip(1)

}

class Day15A extends FlatSpec with Matchers {
  import Day15._

  def solver(startA: Int, startB: Int, length: Int): Int = {

    val iteratorA = genA(startA)
    val iteratorB = genB(startB)

    def matches(a: Long, b: Long): Boolean = ((a ^ b) & 0xffff) == 0

    @tailrec
    def round(remaining: Int, count: Int): Int = {
      if (remaining == 0) {
        count
      }
      else {
        val nextCount = if (matches(iteratorA.next(), iteratorB.next())) count + 1 else count
        round(remaining - 1, nextCount)
      }
    }

    round(length, 0)
  }

  "Solver" should "print result" in {
    println(solver(873, 583, 40000000))
  }

  def streamSolver(startA: Int, startB: Int, length: Int): Int = {
    val iteratorA = streamA(startA).iterator()
    val iteratorB = streamB(startB).iterator()

    def matches(a: Long, b: Long): Boolean = ((a ^ b) & 0xffff) == 0

    @tailrec
    def round(remaining: Int, count: Int): Int = {
      if (remaining == 0) {
        count
      }
      else {
        val nextCount = if (matches(iteratorA.nextLong(), iteratorB.nextLong())) count + 1 else count
        round(remaining - 1, nextCount)
      }
    }

    round(length, 0)
  }

  "LongStream Solver" should "print result" in {
    println(solver(873, 583, 40000000))
  }

  "Generator function" should "pass test vectors" in {
    genA(65).take(5).toSeq should be(Seq(1092455, 1181022009, 245556042, 1744312007, 1352636452))
    genB(8921).take(5).toSeq should be(Seq(430625591, 1233683848, 1431495498, 137874439, 285222916))
    solver(65, 8921, 5) should be(1)
    solver(65, 8921, 40000000) should be(588)
  }
}

class Day15B extends FlatSpec with Matchers {
  import Day15._

  def solver(startA: Int, startB: Int, length: Int): Int = {

    val iteratorA = genA(startA).filter(n => (n & 3) == 0)
    val iteratorB = genB(startB).filter(n => (n & 7) == 0)

    def matches(a: Long, b: Long): Boolean = ((a ^ b) & 0xffff) == 0

    @tailrec
    def round(remaining: Int, count: Int): Int = {
      if (remaining == 0) {
        count
      }
      else {
        val nextCount = if (matches(iteratorA.next(), iteratorB.next())) count + 1 else count
        round(remaining - 1, nextCount)
      }
    }

    round(length, 0)
  }

  "Solver" should "print result" in {
    println(solver(873, 583, 5000000))
  }

  def streamSolver(startA: Int, startB: Int, length: Int): Int = {

    val iteratorA = streamA(startA).filter(n => (n & 3) == 0).iterator()
    val iteratorB = streamB(startB).filter(n => (n & 7) == 0).iterator()

    def matches(a: Long, b: Long): Boolean = ((a ^ b) & 0xffff) == 0

    @tailrec
    def round(remaining: Int, count: Int): Int = {
      if (remaining == 0) {
        count
      }
      else {
        val nextCount = if (matches(iteratorA.nextLong(), iteratorB.nextLong())) count + 1 else count
        round(remaining - 1, nextCount)
      }
    }

    round(length, 0)
  }

  "LongStream Solver" should "print result" in {
    println(streamSolver(873, 583, 5000000))
  }

  "Generator function" should "pass test vectors" in {
    genA(65).filter(n => (n & 3) == 0).take(5).toSeq should be(Seq(1352636452, 1992081072, 530830436, 1980017072, 740335192))
    genB(8921).filter(n => (n & 7) == 0).take(5).toSeq should be(Seq(1233683848, 862516352, 1159784568, 1616057672, 412269392))
    solver(65, 8921, 1056) should be(1)
    solver(65, 8921, 5000000) should be(309)
  }
}

