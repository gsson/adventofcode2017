import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

object Day9 {

  def skipGarbage(chars: Iterator[Char]): String = {
    val sb = StringBuilder.newBuilder
    while (true) {
      val c = chars.next()

      c match {
        case '!' =>
          chars.next()
        case '>' =>
          return sb.toString()
        case _ =>
          sb += c
      }
    }
    sb.toString()
  }

  sealed trait Token
  final case object Begin extends Token
  final case object End extends Token
  final case class Garbage(characters: String) extends Token

  def parse2(chars: Iterator[Char]): Iterator[Token] = {
    new Iterator[Token] {
      private def getNextToken: Option[Token] = {
        while (chars.hasNext) {
          val c = chars.next()
          c match {
            case '{' =>
              return Some(Begin)
            case '}' =>
              return Some(End)
            case '<' =>
              return Some(Garbage(skipGarbage(chars)))
            case _ => ()
          }
        }
        None
      }

      private var nextToken: Option[Token] = getNextToken

      override def hasNext: Boolean = nextToken.isDefined

      override def next(): Token = {
        val result = nextToken.get
        nextToken = getNextToken
        result
      }
    }

  }
}

class Day9A extends FlatSpec with Matchers {
  import Day9._

  "Solver" should "print result" in {
    println(solver(Source.fromResource("Day9.txt").iter))
  }

  def score(depth: Int, agg: Int, tokens: List[Token]): Int = {
    tokens match {
      case Begin :: tail => score(depth + 1, agg, tail)
      case End :: tail => score(depth - 1, agg + depth, tail)
      case Garbage(_) :: tail => score(depth, agg, tail)
      case Nil => agg
    }
  }

  def solver(chars: Iterator[Char]): Int = {
    val tokens = parse2(chars).toList
    score(0, 0, tokens)
  }

  it should "pass test vectors" in {
    solver("{}".iterator) should be(1)
    solver("{{{}}}".iterator) should be(6)
    solver("{{},{}}".iterator) should be(5)
    solver("{{{},{},{{}}}}".iterator) should be(16)
    solver("{<{},{},{{}}>}".iterator) should be(1)
    solver("{<a>,<a>,<a>,<a>}".iterator) should be(1)
    solver("{{<ab>},{<ab>},{<ab>},{<ab>}}".iterator) should be(9)
    solver("{{<!!>},{<!!>},{<!!>},{<!!>}}".iterator) should be(9)
    solver("{{<a!>},{<a!>},{<a!>},{<ab>}}".iterator) should be(3)
  }
}

class Day9B extends FlatSpec with Matchers {
  import Day9._
  def score(agg: Int, tokens: List[Token]): Int = {
    tokens match {
      case Begin :: tail => score(agg, tail)
      case End :: tail => score(agg, tail)
      case Garbage(g) :: tail =>
        score(agg + g.length, tail)
      case Nil => agg
    }
  }

  def solver(chars: Iterator[Char]): Int = {
    val tokens = parse2(chars).toList
    score(0, tokens)
  }

  "Solver" should "print result" in {
    println(solver(Source.fromResource("Day9.txt").iter))
  }

}
