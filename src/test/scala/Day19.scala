import Day13.{Layer, layer}
import Day19.load
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source

object Day19 {
  val H : Int = '-'
  val V : Int = '|'
  val X : Int = '+'
  val N : Int = ' '

  sealed trait Direction {
    def apply(x: Int, y: Int): (Int, Int)
    def apply(c: (Int, Int)): (Int, Int) = apply(c._1, c._2)
  }
  case object Right extends Direction {
    override def apply(x: Int, y: Int): (Int, Int) = (x + 1, y)
  }
  case object Up extends Direction {
    override def apply(x: Int, y: Int): (Int, Int) = (x, y - 1)
  }
  case object Left extends Direction {
    override def apply(x: Int, y: Int): (Int, Int) = (x - 1, y)
  }
  case object Down extends Direction {
    override def apply(x: Int, y: Int): (Int, Int) = (x, y + 1)
  }

  case object Stop extends Direction {
    override def apply(x: Int, y: Int): (Int, Int) = ???
  }

  class Grid(val width: Int, val height: Int, values: Array[Byte]) {
    def apply(x: Int, y: Int): Int = if (x < 0 || x >= width || y < 0 || y >= height) N else values(width * y + x)
    def apply(c: (Int, Int)): Int = apply(c._1, c._2)
    def adjacent(x: Int, y: Int): (Int, Int, Int, Int) = {
      (apply(x + 1, y), apply(x, y - 1), apply(x - 1, y), apply(x, y + 1))
    }
  }

  def load(file: String): Grid = {
    val lines = Source.fromResource(file).getLines().toArray

    val width = lines.map(_.length).max
    val height = lines.length

    val tiles = Array.fill[Byte](width * height)(' ')
    lines.zipWithIndex.foreach {
      case (l, rowIndex) =>
        val rowOffset = rowIndex * width
        l.zipWithIndex.foreach {
          case (c, colIndex) =>
            tiles.update(rowOffset + colIndex, c.toByte)
        }
    }

    new Grid(width, height, tiles)
  }

}

class Day19AB extends FlatSpec with Matchers {
  import Day19._

  def solver(grid: Grid): (Int, String) = {
    val startCol = (0 to grid.width).find(grid(_, 0) == V).get

    @tailrec
    def step(c: (Int, Int), dir: Direction, string: String, steps: Int): (Int, String) = {
      val next = grid(c)
      //println(c, dir, string, next.toChar.toString)

      val (nextString, nextDir) = (dir, next) match {
        case (Left, H) | (Left, V) =>
          (string, Left)
        case (Right, H) | (Right, V) =>
          (string, Right)
        case (Up, H) | (Up, V) =>
          (string, Up)
        case (Down, H) | (Down, V) =>
          (string, Down)

        case (Up, X) | (Down, X) =>
          val l = grid(Left(c))
          if (l != V && l != N)
            (string, Left)
          else {
            val l = grid(Right(c))
            if (l != V && l != N)
              (string, Right)
            else throw new IllegalStateException((c, dir, string).toString())
          }

        case (Left, X) |  (Right, X) =>
          val l = grid(Up(c))
          if (l != H && l != N)
            (string, Up)
          else {
            val l = grid(Down(c))
            if (l != H && l != N)
              (string, Down)
            else throw new IllegalStateException((c, dir, string).toString())
          }


        case (_, N) =>
          (string, Stop)
        case (d, s) =>
          (string + s.toChar, d)
      }

      if (nextDir == Stop) {
        (steps, nextString)
      }
      else {
        step(nextDir(c), nextDir, nextString, steps + 1)
      }
    }
    step((startCol, 0), Down, "", 0)
  }

  "Solver" should "print result" in {
    println(solver(load("Day19.txt")))
  }

  it should "pass test vectors" in {
    solver(load("Day19-test.txt")) should be((38, "ABCDEF"))
  }

}
