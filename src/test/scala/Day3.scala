import org.scalatest.{FlatSpec, Matchers}

class Day3A extends FlatSpec with Matchers {
  def solver(input: Int): Int = {
    if (input == 1) {
      0
    }
    else {
      // Ring "radius" for the input number; inverse of v = 4r^2 + 4r + 1
      val radius = math.ceil((-4 + math.sqrt(16 * input)) / 8).toInt

      // Length of the side of the ring
      val side = 2 * radius + 1

      // Highest number in this ring (right downwards diagonal); 4r^2 + 4r + 1
      val last = 4 * radius * radius + 4 * radius + 1

      // Value offset from the center of the side it is on.
      val offset = math.abs((last - input) % (side - 1) - side / 2)

      radius + offset
    }
  }

  "Solver" should "print result" in {
    println(solver(325489))
  }

  it should "pass test vectors" in {
    solver(1) should be(0)
    solver(12) should be(3)
    solver(23) should be(2)
    solver(1024) should be(31)
  }
}

class Day3B extends FlatSpec with Matchers {
  def coordinatesForRadius(radius: Int): Seq[(Int, Int)] = {
    if (radius == 0) {
      Seq((0, 0))
    }
    else {
      // Length of the side of the ring
      val side = 2 * radius + 1
      val halfSide = side / 2

      Range(-halfSide + 1, halfSide).map((radius, _)) ++
        Range(halfSide, -halfSide, -1).map((_, radius)) ++
        Range(halfSide, -halfSide, -1).map((-radius, _)) ++
        Range(-halfSide, halfSide).map((_, -radius)) ++ Vector((halfSide, -halfSide))
    }
  }

  def adjacent(coordinate: (Int, Int)): Seq[(Int, Int)] = {
    val (x, y) = coordinate
    Seq(
      (x - 1, y - 1),
      (x, y - 1),
      (x + 1, y - 1),
      (x - 1, y),
      (x + 1, y),
      (x - 1, y + 1),
      (x, y + 1),
      (x + 1, y + 1)
    )
  }


  def solver(input: Int): Int = {
    // We know the answer is on or before the radius of the circle containing the input. Add 1 to
    // ensure there are valid 0s to either side to avoid bounds checking in the loop.
    val maxRadius = math.ceil((-4 + math.sqrt(16 * input)) / 8).toInt + 1

    // Length of the side of the ring
    val maxSide = 2 * maxRadius + 1

    // Mutable state \o/
    val data = Array.ofDim[Int](maxSide * maxSide)

    def index(coordinate: (Int, Int)): Int = {
      val (x, y) = coordinate
      (x + maxRadius) + (y + maxRadius) * maxSide
    }

    def sum(coords: Seq[(Int, Int)]): Int = {
      coords.map(index).map(data).sum
    }

    data.update(index((0, 0)), 1)

    Range(1, maxRadius)
      .flatMap(coordinatesForRadius)
      .map { coord =>
        val adj = adjacent(coord)
        val s = sum(adj)
        val i = index(coord)
        data.update(i, s)
        s
      }.find(_ > input).head
  }

  "Solver" should "pass test vectors" in {
    solver(2) should be(4)
    solver(10) should be(11)
    solver(11) should be(23)
    solver(23) should be(25)
    solver(750) should be(806)
    solver(805) should be(806)
    solver(806) should be(880)
  }

  it should "print result" in {
    println(solver(325489))
  }

}
