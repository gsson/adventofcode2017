import Day24.{ComponentIndex, ComponentSet, Components, load}
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.collection.{IndexedSeqOptimized, mutable}
import scala.io.Source

object Day24 {
  object Component {
    val Nil = new Component(-1)
    def apply(a: Int, b: Int): Component = {
      Predef.assert(a < Byte.MaxValue)
      Predef.assert(b < Byte.MaxValue)

      if (a >= b) {
        new Component(((a << 8) | b).toShort)
      }
      else {
        new Component(((b << 8) | a).toShort)
      }
    }
  }

  class Component(val encoded: Short) extends AnyVal {
    def a: Int = encoded.toInt & 0xff
    def b: Int = (encoded.toInt >>> 8) & 0xff

    def score: Int = a + b

    def opposite(n: Int) = if (a != n) a else b

    override def toString: String = s"$a/$b"
  }

  class ComponentsBuilder extends mutable.Builder[Component, Components] {
    private val arrayBuilder: mutable.ArrayBuilder[Short] = new mutable.ArrayBuilder.ofShort()
    override def +=(elem: Component): ComponentsBuilder.this.type = {
      arrayBuilder.+=(elem.encoded)
      this
    }

    override def clear(): Unit = arrayBuilder.clear()

    override def result(): Components = new Components(arrayBuilder.result())

    override def sizeHint(size: Int): Unit = arrayBuilder.sizeHint(size)
  }

  object Components {
    val empty: Components = new Components(Array.emptyShortArray)

    def newBuilder: ComponentsBuilder = new ComponentsBuilder

    def apply(components: Component*): Components = {
      val b = newBuilder
      components.foreach(b += _)
      b.result()
    }
    def apply(components: Iterator[Component]): Components = {
      val b = newBuilder
      components.foreach(b += _)
      b.result()
    }
  }

  final class Components(val components: Array[Short]) extends IndexedSeq[Component] with IndexedSeqOptimized[Component, Components] {
    def length: Int = components.length
    def apply(i: Int): Component = new Component(components(i))

    override protected[this] def newBuilder: mutable.Builder[Component, Components] = Components.newBuilder

    override def seq: IndexedSeq[Component] = this
  }

  object ComponentIndex {
    def apply(components: Components) = {
      Predef.assert(components.length < Byte.MaxValue)
      val ag = components.zipWithIndex.foldLeft(Map.empty[Int, Long]) {
        case (map, (comp, i)) => map + (
          comp.a -> (map.getOrElse(comp.a, 0L) | (1L << i)),
          comp.b -> (map.getOrElse(comp.b, 0L) | (1L << i)))
      }
      val height = ag.keys.max

      val index = Range.inclusive(0, height)
        .map(ag.getOrElse(_, 0L))
        .toArray

      new ComponentIndex(index)
    }
  }

  object ComponentSet {
    def empty: ComponentSet = new ComponentSet(0L)
    def all(implicit components: Components) = new ComponentSet((1L << components.size) - 1)

    @tailrec
    def foreachBit[@specialized U](n: Long, f: Int => U): Unit = {
      if (n != 0L) {
        val bit = java.lang.Long.lowestOneBit(n)
        f(java.lang.Long.numberOfTrailingZeros(bit))
        foreachBit(n ^ bit, f)
      }
    }
    @tailrec
    def foldLeftBits[@specialized U](v: U, n: Long, f: (U, Int) => U): U = {
      if (n != 0L) {
        val bit = java.lang.Long.lowestOneBit(n)
        val v2 = f(v, java.lang.Long.numberOfTrailingZeros(bit))
        foldLeftBits(v2, n ^ bit, f)
      } else {
        v
      }
    }
    @tailrec
    def filterBit(from: Long, to: Long, f: Int => Boolean): Long = {
      if (from == 0L) {
        to
      }
      else {
        val bit = java.lang.Long.lowestOneBit(from)
        val nextTo = if (f(java.lang.Long.numberOfTrailingZeros(bit))) to | bit else to
        filterBit(from ^ bit, nextTo, f)
      }
    }

  }

  class ComponentSet(val n: Long) extends AnyVal {
    def isEmpty: Boolean = n == 0L

    def score(implicit components: Components): Int = foldLeft(0)(_ + _.score)

    def foreach[@specialized U](f: Int => U): Unit = {
      ComponentSet.foreachBit(n, i => f(i))
    }

    def foldLeft[@specialized U](init: U)(f: (U, Int) => U): U = {
      ComponentSet.foldLeftBits(init, n, (v, i) => f(v, i))
    }

    def length: Int = java.lang.Long.bitCount(n)

    def intersect(that: ComponentSet): ComponentSet = new ComponentSet(n & that.n)
    def union(that: ComponentSet): ComponentSet = new ComponentSet(n | that.n)
    def diff(that: ComponentSet): ComponentSet = new ComponentSet(n & ~that.n)

    def `-`(that: Int): ComponentSet = this.diff(new ComponentSet(1L << that))
    def `+`(that: Int): ComponentSet = this.union(new ComponentSet(1L << that))
    def `&`(that: Int): ComponentSet = this.intersect(new ComponentSet(1L << that))

    def `-`(that: ComponentSet): ComponentSet = diff(that)
    def `|`(that: ComponentSet): ComponentSet = union(that)
    def `&`(that: ComponentSet): ComponentSet = intersect(that)

    def toArray: Array[Int] = {
      val b = new mutable.ArrayBuilder.ofInt()
      b.sizeHint(length)
      ComponentSet.foreachBit(n, b.+=)
      b.result()
    }

    override def toString: String = {
      toArray.mkString("[", ", ", "]")
    }
  }

  class ComponentIndex(index: Array[Long]) {
    def apply(i: Int): ComponentSet = {
      new ComponentSet(index(i))
    }

    override def toString: String = {
      index.indices
        .filter(index(_) != 0)
        .map(i => i -> apply(i))
        .toString
    }
  }

  val P = raw"(\d+)/(\d+)".r
  def load(file: String): Components = {
    val c = Source.fromResource(file).getLines().map {
      case P(a, b) => Component(a.toInt, b.toInt)
    }
    Components(c)
  }
}

class Day24A extends FlatSpec with Matchers {

  import Day24._
  def solve(size: Int, available: ComponentSet, members: ComponentSet)(implicit components: Components, componentIndex: ComponentIndex): Int = {
    val candidates = available & componentIndex(size)
    if (candidates.isEmpty) {
      members.foldLeft(0) { (n, ci) => n + ci.score }
    }
    else {
      candidates.foldLeft(0) { (n, ci) =>
        Math.max(n, solve(ci.opposite(size), available - ci, members + ci))
      }
    }
  }

  def solve(implicit components: Components): Int = {
    implicit val componentIndex = ComponentIndex(components)
    solve(0, ComponentSet.all, ComponentSet.empty)
  }
  
  "Solver" should "print result" in {
    val p = load("Day24.txt")

    println(solve(p))
  }

  it should "pass test vectors" in {
    val p = load("Day24-test.txt")

    solve(p) should be(31)
  }
}


class Day24B extends FlatSpec with Matchers {
  def solve(size: Int, available: ComponentSet, members: ComponentSet)(implicit components: Components, componentIndex: ComponentIndex): ComponentSet = {
    val candidates = available & componentIndex(size)
    if (candidates.isEmpty) {
      members
    }
    else {
      candidates.foldLeft(ComponentSet.empty) { (set, ci) =>
        val cset = solve(ci.opposite(size), available - ci, members + ci)
        val l = cset.length - set.length
        if (l > 0 || (l == 0 && cset.score > set.score)) {
          cset
        }
        else {
          set
        }
      }
    }
  }

  def solve(implicit components: Components): Int = {
    implicit val componentIndex = ComponentIndex(components)
    solve(0, ComponentSet.all, ComponentSet.empty).score
  }


  "Solver" should "print result" in {
    val p = load("Day24.txt")

    println(solve(p))
  }

  it should "pass test vectors" in {
    val p = load("Day24-test.txt")

    solve(p) should be(19)

  }


}