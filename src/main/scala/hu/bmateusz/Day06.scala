package hu.bmateusz

object Day06 {

  /** https://adventofcode.com/2019/day/7 */
  def main(args: Array[String]): Unit = {
    val input: Seq[Edge] = FileOperations
      .readResourceLines("day06.txt")
      .map(Edge.fromString)
    println(countOrbits(input))
    println(fromYouToSanta(input))
  }

  val centerOfMass = "COM"
  val you = "YOU"
  val santa = "SAN"

  class Edge(val from: String, val to: String)

  object Edge {
    def fromString(string: String): Edge = {
      val sp = string.split(')')
      new Edge(sp(0), sp(1))
    }
  }

  def countOrbits(input: Seq[Edge],
                  from: String = centerOfMass,
                  indirect: Int = 0): Int = {
    input
      .filter(_.from == from)
      .map(e => indirect + countOrbits(input, e.to, indirect + 1))
      .sum + (if (from == centerOfMass) 0 else 1)
  }

  def findWay(input: Seq[Edge],
              to: String,
              from: String = centerOfMass,
              steps: Seq[String] = Seq.empty): Seq[String] = {
    if (from == to) {
      steps :+ to
    } else {
      input
        .filter(_.from == from)
        .flatMap(e => findWay(input, to, e.to, steps :+ e.from))
    }
  }

  def fromYouToSanta(input: Seq[Edge]): Int = {
    val toYou = findWay(input, you)
    val toSanta = findWay(input, santa)
    val common = toYou.intersect(toSanta)
    toYou.length + toSanta.length - common.length * 2 - 2
  }

}
