import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.io.Source

object Day20 {
  def sameSign(a: Int, b: Int): Boolean = a == 0 || b == 0 || ((a ^ b) & 0x80000000) == 0
  def sameSign(v1: Vec3, v2: Vec3): Boolean = sameSign(v1.x, v2.x) && sameSign(v1.y, v2.y) && sameSign(v1.z, v2.z)

  object Vec3 {
    implicit val Order: Ordering[Vec3] = Ordering.by[Vec3, (Int, Int, Int)](v => (v.x, v.y, v.z))
  }
  case class Vec3(x: Int, y: Int, z: Int) {
    def `+`(that: Vec3): Vec3 = Vec3(x + that.x, y + that.y, z + that.z)
    def `-`(that: Vec3): Vec3 = Vec3(x - that.x, y - that.y, z - that.z)

    def manhattanLength: Int = Math.abs(x) + Math.abs(y) + Math.abs(z)

    override def toString: String = s"<$x,$y,$z>"
  }

  object Solution {
    def apply(): Solution = None
    def apply(t: Int): Solution = Single(t)
    def apply(t1: Int, t2: Int): Solution = if (t1 == t2) Single(t1) else if (t1 < t2) Double(t1, t2) else Double(t2, t1)
  }
  sealed trait Solution {
    def intersect(solution: => Solution): Solution
    def filter(f: Int => Boolean): Solution
    def toSeq: Seq[Int]
  }
  final case object None extends Solution {
    override def intersect(solution: => Solution) = None
    override def filter(f: Int => Boolean): Solution = None
    override def toSeq: Seq[Int] = Seq.empty[Int]
  }
  final case class Single(t: Int) extends Solution {
    override def intersect(solution: => Solution) = solution match {
      case Single(tt) if t == tt => this
      case Double(tt1, _) if t == tt1 => this
      case Double(_, tt2) if t == tt2 => this
      case Infinite => this
      case _ => None
    }
    override def filter(f: Int => Boolean): Solution = if (f(t)) this else None
    override def toSeq: Seq[Int] = Seq(t)
  }
  final case class Double(t1: Int, t2: Int) extends Solution {
    override def intersect(solution: => Solution) = solution match {
      case s @ Single(tt) if t1 == tt || t2 == tt => s
      case Double(tt1, tt2) =>
        if (t1 == tt1 && t2 == tt2)
          this
        else if (t1 == tt1 || t1 == tt2)
          Single(t1)
        else if (t2 == tt1 || t2 == tt2)
          Single(t2)
        else
          None
      case Infinite => this
      case _ => None
    }
    override def filter(f: Int => Boolean): Solution = {
      val a1 = f(t1)
      val a2 = f(t2)
      if (a1 && a2) {
        this
      }
      else if (a1) {
        Single(t1)
      }
      else if (a2) {
        Single(t2)
      }
      else None
    }
    override def toSeq: Seq[Int] = Seq(t1, t2)
  }

  final case object Infinite extends Solution {
    override def intersect(solution: => Solution) = solution
    override def filter(f: Int => Boolean): Solution = ???
    override def toSeq: Seq[Int] = ???
  }

  object Curve {
    def apply(a: Int, b: Int, c: Int): Curve = {
      if (a == 0) {
        if (b == 0)
          Point(c)
        else
          Linear(b, c)
      }
      else {
        Quadratic(a, b, c)
      }
    }
  }
  sealed trait Curve {
    def roots: Solution
  }
  final case class Point(c: Int) extends Curve {
    def roots: Solution = Infinite
  }
  final case class Linear(b: Int, c: Int) extends Curve {
    def roots: Solution = Single(-2 * c / b)
  }
  final case class Quadratic(a: Int, b: Int, c: Int) extends Curve {
    def roots: Solution = {
      val D = b * b - 8 * a * c
      if (D > 0) {
        val d = Math.sqrt(D).toInt
        val s1 = (-b + d) / (2 * a)
        val s2 = (-b - d) / (2 * a)

        Solution(s1, s2)
      }
      else if (D == 0) {
        val s = -b / (2 * a)
        Solution(s)
      }
      else {
        Solution()
      }
    }
  }

  object Particle {
    def abc(pos: Int, vel: Int, acc: Int): Curve = {
      val a = acc
      val b = 2 * vel + acc
      val c = pos
      Curve(a, b, c)
    }

    def collides(part1: Particle, part2: Particle): Solution = {
      val dpos = part2.p - part1.p
      val dvel = part2.v - part1.v
      val dacc = part2.a - part1.a

      abc(dpos.x, dvel.x, dacc.x).roots
        .intersect(abc(dpos.y, dvel.y, dacc.y).roots)
        .intersect(abc(dpos.z, dvel.z, dacc.z).roots)
        .filter(t => part1.at(t) == part2.at(t))
    }

    def axisAt(t: Int, pos: Int, vel: Int, acc: Int): Int = pos + ((2 * vel + acc) * t + acc * t * t) / 2

  }

  case class Particle(id: Int, p: Vec3, v: Vec3, a: Vec3) {
    override def toString: String = s"p=$p, v=$v, a=$a"

    def `+`(that: Particle): Particle = Particle(-1, p + that.p, v + that.v, a + that.a)
    def `-`(that: Particle): Particle = Particle(-1, p - that.p, v - that.v, a - that.a)

    def tick: Particle = {
      val v2 = v + a
      val p2 = p + v2
      Particle(id, p2, v2, a)
    }

    def at(t: Int): Vec3 = {
      Vec3(
        Particle.axisAt(t, p.x, v.x, a.x),
        Particle.axisAt(t, p.y, v.y, a.y),
        Particle.axisAt(t, p.z, v.z, a.z)
      )
    }

    def distance: Int = p.manhattanLength


    def mightGetCloser: Boolean = (Integer.signum(p.x) != Integer.signum(v.x)) || (a.x != 0 && Integer.signum(v.x) != Integer.signum(a.x))
  }

  val P = raw"p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>".r

  def load(file: String): IndexedSeq[Particle] = {
    Source.fromResource(file).getLines().zipWithIndex.map {
      case (s, i) => s match {
        case P(px, py, pz, vx, vy, vz, ax, ay, az) => Particle(i,
          Vec3(px.toInt, py.toInt, pz.toInt),
          Vec3(vx.toInt, vy.toInt, vz.toInt),
          Vec3(ax.toInt, ay.toInt, az.toInt))
      }
    }.toArray[Particle]
  }

}

class Day20A extends FlatSpec with Matchers {

  import Day20._

  def solver(particles: IndexedSeq[Particle]): Int = {
    def forwardVelocity(particle: Particle): Int = {
      val a = particle.a
      @tailrec
      def f(v: Vec3): Int = {
        if (sameSign(a, v)) {
          v.manhattanLength
        }
        else {
          f(v + a)
        }
      }

      f(particle.v)
    }

    def forwardPosition(particle: Particle): Int = {
      val a = particle.a
      @tailrec
      def f(p: Vec3, v: Vec3): Int = {
        if (sameSign(a, p) && sameSign(a, v)) {
          p.manhattanLength
        }
        else {
          val v2 = v + a
          f(p + v2, v2)
        }
      }

      f(particle.p, particle.v)
    }

    val lowestAcceleration = particles.foldLeft((Int.MaxValue, Seq.empty[Particle])) {
      case ((min, seq), part) if part.a.manhattanLength > min =>
        (min, seq)
      case ((min, seq), part) if part.a.manhattanLength == min =>
        (min, part +: seq)
      case ((min, seq), part) if part.a.manhattanLength < min =>
        (part.a.manhattanLength, Seq(part))
    }._2

    println(lowestAcceleration)

    val lowestVelocity = lowestAcceleration.foldLeft((Int.MaxValue, Seq.empty[Particle])) {
      case ((min, seq), part) =>
        val fv = forwardVelocity(part)
        if (min < fv) {
          (min, seq)
        }
        else if (min == fv) {
          (min, part +: seq)
        }
        else {
          (fv, Seq(part))
        }
    }._2

    println(lowestVelocity)

    val lowestPos = lowestVelocity.foldLeft((Int.MaxValue, Seq.empty[Particle])) {
      case ((min, seq), part) =>
        val fp = forwardPosition(part)
        if (min < fp) {
          (min, seq)
        }
        else if (min == fp) {
          (min, part +: seq)
        }
        else {
          (fp, Seq(part))
        }
    }._2

    println(lowestPos)
    lowestPos.head.id
  }

  "Solver" should "print result" in {
    val particles = load("Day20.txt")
    val res = solver(particles)
    println(res)
  }

  it should "pass test vectors" in {
    solver(load("Day20a-test.txt")) should be(0)
  }
}

class Day20B extends FlatSpec with Matchers {

  import Day20.Vec3.Order
  import Day20._

  def collapseCollisions(collisions: Seq[(Int, Int)]): Set[Int] = collisions.flatMap { case (a, b) => Seq(a, b) }.toSet

  def solve(particles: IndexedSeq[Particle]): Set[Particle] = {
    val collisions = particles.indices.flatMap { a =>
      val pa = particles(a)
      Range(a + 1, particles.length).flatMap { b =>
        val pb = particles(b)
        Particle.collides(pa, pb).toSeq.map { t => (t, pa.at(t)) -> (a, b) }
      }
    }
      .groupBy(_._1)
      .mapValues(c => collapseCollisions(c.map(_._2)))

    val orderedCollisions = TreeMap(collisions.toSeq :_*)

    val (rem, _) = orderedCollisions.valuesIterator.foldLeft((particles, Set.empty[Int])) { (prev, collision) =>
      val (rem, rmv) = prev
      val remainingCollision = collision -- rmv
      if (remainingCollision.size > 1) {
        (rem.filterNot(p => remainingCollision.contains(p.id)), rmv ++ remainingCollision)
      }
      else {
        prev
      }
    }

    rem.toSet
  }


  "Solver" should "print result" in {
    val particles = load("Day20.txt")
    val remaining = solve(particles)
    println(remaining.size)
  }


  it should "pass test vectors" in {
    val particles = load("Day20b-test.txt")

    val remaining = solve(particles)
    remaining.size should be(1)
  }
}