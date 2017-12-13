import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source

object Day13 {

  case class Layer(depth: Int, range: Int) {
    val period = (range - 1) * 2

    def catches: Boolean = catches(0)

    def catches(delay: Int): Boolean = (depth + delay) % period == 0

    def score: Int = depth * range
  }

  val pattern = raw"(\d+):\s+(\d+)".r

  def layer(line: String): Layer = {
    line match {
      case pattern(depth, range) =>
        Layer(depth.toInt, range.toInt)
    }
  }

  def load(file: String): Seq[Layer] = {
    Source.fromResource(file).getLines()
      .map(layer)
      .toList
  }
}

class Day13A extends FlatSpec with Matchers {
  import Day13._

  def solver(layers: Seq[Layer]): Int = {
    layers
      .filter(_.catches)
      .map(_.score)
      .sum
  }

  "Solver" should "print result" in {
    val layers = load("Day13.txt")

    println(solver(layers))
  }

  it should "pass test vectors" in {
    val layers = load("Day13-test.txt")

    solver(layers) should be(24)
  }
}

class Day13B extends FlatSpec with Matchers {
  import Day13._

  def solver(layers: Seq[Layer]): Option[Int] = {
    def caught(delay: Int) = layers.exists(l => l.catches(delay))

    Iterator.from(0).find(delay => !caught(delay))
  }



  "Solver" should "print result" in {
    val layers = load("Day13.txt")

    println(solver(layers).get)
  }

  it should "pass test vectors" in {
    val layers = load("Day13-test.txt")

    solver(layers) should be(Some(10))
  }
}
