import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source

object Day11 {
  object Direction {
    val all = Seq(N, NE, SE, S, SW, NW)
  }
  sealed trait Direction
  case object N extends Direction
  case object NE extends Direction
  case object SE extends Direction
  case object S extends Direction
  case object SW extends Direction
  case object NW extends Direction

  def combine(a: Direction, b: Direction, map: Map[Direction, Int]): Map[Direction, Int] = {
    def min(a: Direction, b: Direction) = math.min(map(a), map(b))
    def cancel(a: Direction, b: Direction) = {
      val n = min(a, b)
      map + (a -> (map(a) - n)) + (b -> (map(b) - n))
    }

    def replace(a: Direction, b: Direction, c: Direction) = {
      val n = min(a, b)
      map + (a -> (map(a) - n)) + (b -> (map(b) - n)) + (c -> (map(c) + n))
    }

    (a, b) match {
      case (N, S) | (S, N) => cancel(N, S)
      case (NE, SW) | (SW, NE) => cancel(NE, SW)
      case (NW, SE) | (SE, NW) => cancel(NW, SE)
      case (NW, NE) | (NE, NW) => replace(NW, NE, N)
      case (SW, SE) | (SE, SW) => replace(SW, SE, S)
      case (NE, S) | (S, NE) => replace(NE, S, SE)
      case (NW, S) | (S, NW) => replace(NW, S, SW)
      case (SE, N) | (N, SE) => replace(SE, N, NE)
      case (SW, N) | (N, SW) => replace(SW, N, NW)
      case _ => map
    }
  }

  @tailrec
  def optimise(map: Map[Direction, Int]): Map[Direction, Int] = {
    val nextMap = Direction.all.combinations(2).foldLeft(map) {
      case (m, Seq(a, b)) => combine(a, b, m)
    }
    if (nextMap == map) {
      map
    }
    else {
      optimise(nextMap)
    }
  }

  def distance(steps: Seq[Direction]): Int = {
    val countSteps = steps
      .aggregate(Map.empty[Direction, Int].withDefaultValue(0))((map, d) => map.updated(d, map(d) + 1), _ ++ _)

    optimise(countSteps).values.sum
  }

  def maxDistance(steps: Seq[Direction]): Int = {
    steps.scanLeft(Map.empty[Direction, Int].withDefaultValue(0))((map, d) => optimise(map.updated(d, map(d) + 1))).map(_.values.sum).max
  }

  def load(file: String): Seq[Direction] = {
    Source.fromResource(file).getLines()
      .flatMap(_.split(','))
      .map {
        case "n" => N
        case "ne" => NE
        case "nw" => NW
        case "s" => S
        case "se" => SE
        case "sw" => SW
      }
      .toList
  }
}

class Day11A extends FlatSpec with Matchers {
  import Day11._
  "Solver" should "print result" in {
    println(distance(load("Day11.txt")))
  }

  it should "pass test vectors" in {
    distance(Seq(NE, NE, NE)) should be(3)
    distance(Seq(NE, NE, SW, SW)) should be(0)
    distance(Seq(NE, NE, S, S)) should be(2)
    distance(Seq(SE, SW, SE, SW, SW)) should be(3)
  }
}

class Day11B extends FlatSpec with Matchers {
  import Day11._

  "Solver" should "print result" in {
    val steps = load("Day11.txt")
    println(maxDistance(steps))
  }
}
