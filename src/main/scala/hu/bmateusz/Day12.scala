package hu.bmateusz

import scala.annotation.tailrec

object Day12 {

  /** https://adventofcode.com/2019/day/12 */
  def main(args: Array[String]): Unit = {
    val input = Vector(
      Coordinate(x = -19, y = -4, z = 2),
      Coordinate(x = -9, y = 8, z = -16),
      Coordinate(x = -4, y = 5, z = -11),
      Coordinate(x = 1, y = 9, z = -13)
    ).map(moonWithInitialVelocity)

    println(stepMoons(input, 1000).map(_.totalEnergy).sum)
    println(spinUntilEquals(stepMoonsUntilMatch(input)))
  }

  case class Coordinate(x: Int, y: Int, z: Int) {
    def abs: Int = Math.abs(x) + Math.abs(y) + Math.abs(z)

    def negate: Coordinate = Coordinate(-x, -y, -z)
  }

  val origo: Coordinate = Coordinate(0, 0, 0)

  def moonWithInitialVelocity(coordinate: Coordinate): Moon = Moon(coordinate, origo)

  def signum(a: Int, b: Int): Int = {
    if (a < b) -1 else if (a > b) 1 else 0
  }

  def plus(a: Coordinate, b: Coordinate): Coordinate = {
    Coordinate(
      a.x + b.x,
      a.y + b.y,
      a.z + b.z
    )
  }

  def calculateVelocity(moon: Moon, curr: Moon): Coordinate = {
    Coordinate(
      signum(moon.position.x, curr.position.x),
      signum(moon.position.y, curr.position.y),
      signum(moon.position.z, curr.position.z)
    )
  }

  def findMatchingCoordinates(states: Map[Vector[(Int, Int)], Long], elem: Vector[(Int, Int)]): Option[Long] =
    states.get(elem)

  case class Result(x: Option[(Long, Long)] = None, y: Option[(Long, Long)] = None, z: Option[(Long, Long)] = None) {
    def addX(index: Option[Long], int: Long): Result =
      index.map(idx => Result(Some(x.getOrElse((idx, int))), y, z)).getOrElse(this)

    def addY(index: Option[Long], int: Long): Result =
      index.map(idx => Result(x, Some(y.getOrElse((idx, int))), z)).getOrElse(this)

    def addZ(index: Option[Long], int: Long): Result =
      index.map(idx => Result(x, y, Some(z.getOrElse((idx, int))))).getOrElse(this)

    def finished: Boolean = x.isDefined && y.isDefined && z.isDefined

    def getX: Long = x.get._2

    def getY: Long = y.get._2

    def getZ: Long = z.get._2

    def allEquals: Boolean = getX == getY && getY == getZ
  }

  @tailrec
  def gcd(a: Long, b: Long): Long =
    if (b == 0) a.abs else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long =
    (a * b).abs / gcd(a, b)

  def spinUntilEquals(result: Result): Long =
    lcm(lcm(result.getX, result.getY), result.getZ)

  @tailrec
  def stepMoonsUntilMatch(last: Vector[Moon],
                          n: Long = 0,
                          stateX: Map[Vector[(Int, Int)], Long] = Map.empty,
                          stateY: Map[Vector[(Int, Int)], Long] = Map.empty,
                          stateZ: Map[Vector[(Int, Int)], Long] = Map.empty,
                          result: Result = Result()): Result = {
    val next = stepMoons(last)
    val xs = next.map(m => (m.position.x, m.velocity.x))
    val ys = next.map(m => (m.position.y, m.velocity.y))
    val zs = next.map(m => (m.position.z, m.velocity.z))
    val newResult = result
      .addX(findMatchingCoordinates(stateX, xs), n)
      .addY(findMatchingCoordinates(stateY, ys), n)
      .addZ(findMatchingCoordinates(stateZ, zs), n)

    if (newResult.finished) {
      newResult
    } else {
      stepMoonsUntilMatch(next, n + 1, stateX + (xs -> n), stateY + (ys -> n), stateZ + (zs -> n), newResult)
    }
  }

  @tailrec
  def stepMoons(moons: Vector[Moon], steps: Int): Vector[Moon] =
    if (steps < 1)
      moons
    else
      stepMoons(stepMoons(moons), steps - 1)

  def stepMoons(moons: Vector[Moon]): Vector[Moon] = {
    moons.map { moon =>
      val gravity = moons.foldLeft(origo) {
        case (acc, curr) =>
          plus(calculateVelocity(curr, moon), acc)
      }
      moon.step(gravity)
    }
  }

  case class Moon(position: Coordinate, velocity: Coordinate) {
    def step(gravity: Coordinate): Moon = {
      val newVelocity = plus(velocity, gravity)
      Moon(plus(position, newVelocity), newVelocity)
    }

    def potentialEnergy: Int = position.abs

    def kineticEnergy: Int = velocity.abs

    def totalEnergy: Int = potentialEnergy * kineticEnergy
  }

}
