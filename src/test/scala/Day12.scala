import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source

object Day12 {
  case class Pid(value: Int) extends AnyVal

  type Connection = (Pid, Pid)
  type Connections = Map[Pid, Set[Pid]]

  val pattern = raw"(\d+) <-> (\d+(?:[^\d]+\d+)*)".r
  def line(line: String): Seq[Connection] = {
    line match {
      case pattern(src, dests) =>
        dests
          .split(raw"[^\d]+")
          .map(dest => Pid(src.toInt) -> Pid(dest.toInt))
          .toSeq
    }
  }

  def transitiveConnections(pid: Pid, connections: Connections): Set[Pid] = {
    @tailrec
    def visit(pid: Pid, remaining: Set[Pid], visited: Set[Pid]): Set[Pid] = {
      val connectedTo = connections(pid)
      val nextVisited = visited + pid
      val nextRemaining = (remaining ++ connectedTo) -- nextVisited

      if (nextRemaining.isEmpty) {
        nextVisited
      }
      else {
        visit(nextRemaining.head, nextRemaining.tail, nextVisited)
      }
    }
    visit(pid, Set.empty, Set.empty)
  }

  def groups(connections: Connections): Seq[Set[Pid]] = {
    @tailrec
    def visitGroups(pid: Pid, remaining: Set[Pid], visited: Set[Pid], groups: Seq[Set[Pid]]): Seq[Set[Pid]] = {
      val group = transitiveConnections(pid, connections)
      val nextVisited = visited ++ group
      val nextGroups = groups :+ group
      val nextRemaining = remaining -- group
      if (nextRemaining.isEmpty) {
        nextGroups
      }
      else {
        visitGroups(nextRemaining.head, nextRemaining.tail, nextVisited, nextGroups)
      }
    }

    val pids = connections.keySet

    visitGroups(pids.head, pids.tail, Set.empty, Seq.empty)
  }

  def load(file: String): Connections = {
    Source.fromResource(file).getLines()
      .flatMap(line)
      .foldLeft[Connections](Map.empty[Pid, Set[Pid]]) {
        case (c, (first, second)) =>
            c.updated(first, c.getOrElse(first, Set.empty) + second)
              .updated(second, c.getOrElse(second, Set.empty) + first)
      }
  }
}

class Day12A extends FlatSpec with Matchers {
  import Day12._
  "Solver" should "print result" in {
    println(transitiveConnections(Pid(0), load("Day12.txt")).size)
  }

  it should "pass test vectors" in {
    transitiveConnections(Pid(0), load("Day12-test.txt")).size should be(6)
  }
}

class Day12B extends FlatSpec with Matchers {
  import Day12._


  "Solver" should "print result" in {
    val connections = load("Day12.txt")
    println(groups(connections).length)
  }

}
