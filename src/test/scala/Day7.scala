import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source


object Day7 {
  case class Program(name: String, weight: Int, aggregateWeight: Int, children: Seq[Program])
  case class Entry(name: String, weight: Int, children: Seq[String])

  def buildProgram(entries: Seq[Entry]): Program = {
    def entriesByName(entries: Seq[Entry], byName: Map[String, Entry]): Map[String, Entry] = {
      entries.foldLeft(Map.empty[String, Entry])((m, e) => m + (e.name -> e))
    }

    def createChildPrograms(children: Seq[String], entries: Map[String, Entry], programRoots: Map[String, Program]): (Int, Seq[Program], Map[String, Entry], Map[String, Program]) = {

      children
        .foldLeft((0, Seq.empty[Program], entries, programRoots)) { (state, name) =>
          val (newProgram, newEntries, newProgramRoots) = createProgram(name, state._3, state._4)
          (state._1 + newProgram.aggregateWeight, state._2 :+ newProgram, newEntries, newProgramRoots)
        }
    }

    def createProgram(name: String, entries: Map[String, Entry], programRoots: Map[String, Program]): (Program, Map[String, Entry], Map[String, Program]) = {
      if (programRoots.contains(name)) {
        (programRoots(name), entries, programRoots - name)
      }
      else {
        val entry = entries(name)

        val (childWeights, childPrograms, remainingEntries, remainingProgramRoots) = createChildPrograms(entry.children, entries - name, programRoots)
        val aggregateWeight = entry.weight + childWeights
        val newProgram = Program(name, entry.weight, aggregateWeight, childPrograms)
        (newProgram, remainingEntries, remainingProgramRoots)
      }
    }

    @tailrec
    def aggregatePrograms(entries: Map[String, Entry], programRoots: Map[String, Program]): Program = {
      if (entries.isEmpty) {
        assert(programRoots.size == 1)
        programRoots.head._2
      }
      else {
        val (name, entry) = entries.head
        val (childWeights, childPrograms, remainingEntries, remainingPrograms) = createChildPrograms(entry.children, entries.tail, programRoots)

        val aggregateWeight = entry.weight + childWeights
        val newProgramRoots = remainingPrograms + (name -> Program(name, entry.weight, aggregateWeight, childPrograms))
        aggregatePrograms(remainingEntries, newProgramRoots)
      }
    }

    aggregatePrograms(entriesByName(entries, Map.empty), Map.empty)
  }

  def load(file: String): Seq[Entry] = {
    val node = raw"""^(\w+) \((\d+)\)(?: -> (.*))?""".r
    Source.fromResource(file).getLines()
      .map {
        case node(name, weight, children) =>
          Day7.Entry(name, weight.toInt, Option(children).toList.flatMap(_.split(raw"\s*,\s*").toList))
      }.toSeq
  }
}

class Day7A extends FlatSpec with Matchers {
  import Day7._
  def solver(entries: Seq[Entry]): String = {
    buildProgram(entries).name
  }

  "Solver" should "print result" in {
    val entries = load("Day7.txt")
    println(solver(entries))
  }

  it should "pass test vectors" in {
    val entries = load("Day7-test.txt")
    solver(entries) should be("tknk")
  }
}

class Day7B extends FlatSpec with Matchers {
  import Day7._

  def findImbalance(program: Program): Option[(Int, Program)] = {
    val grouped = program.children.groupBy(_.aggregateWeight)
    if (grouped.size == 1) {
      None
    }
    else {
      val candidate = grouped.minBy(_._2.length)._2.head
      val siblings = grouped.maxBy(_._2.length)._2
      val correctedWeight = candidate.weight - candidate.aggregateWeight + siblings.head.aggregateWeight
      findImbalance(candidate).orElse(Some((correctedWeight, candidate)))
    }
  }

  def solver(entries: Seq[Entry]) = {
    val program = buildProgram(entries)
    findImbalance(program).get._1
  }

  "Solver" should "print result" in {
    val entries = load("Day7.txt")

    println(solver(entries))
  }

  it should "pass test vectors" in {
    val entries = load("Day7-test.txt")

    solver(entries) should be(60)
  }
}
