import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day4A extends FlatSpec with Matchers {
  def isValid(passphrase: String) = {
    val words = passphrase.split(' ')
    words.length == words.toSet.size
  }

  def solver(input: Iterator[String]): Int = {
    input.count(isValid)
  }

  "Solver" should "print result" in {
    val lines = Source.fromResource("Day4.txt").getLines()

    println(solver(lines))
  }

  it should "pass test vectors" in {
    solver(Seq("aa bb cc dd ee", "aa bb cc dd aa", "aa bb cc dd aaa").iterator) should be(2)
  }
}

class Day4B extends FlatSpec with Matchers {
  def isValid(passphrase: String) = {
    val words = passphrase.split(' ')
    val wordsAndAnagrams = words.flatMap(_.permutations)
    wordsAndAnagrams.length == wordsAndAnagrams.toSet.size
  }

  def solver(input: Iterator[String]): Int = {
    input.count(isValid)
  }

  "Solver" should "print result" in {
    val lines = Source.fromResource("Day4.txt").getLines()

    println(solver(lines))
  }

  it should "pass test vectors" in {
    isValid("abcde fghij") should be(true)
    isValid("abcde xyz ecdab") should be(false)
    isValid("a ab abc abd abf abj") should be(true)
    isValid("iiii oiii ooii oooi oooo") should be(true)
    isValid("oiii ioii iioi iiio") should be(false)
    solver(Seq("aa bb cc dd ee", "aa bb cc dd aa", "aa bb cc dd aaa").iterator) should be(2)
  }
}

