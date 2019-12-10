package hu.bmateusz

object Day10 {

  /** https://adventofcode.com/2019/day/10 */
  def main(args: Array[String]): Unit = {
    val input = Matrix(FileOperations.readResourceLines("day10.txt"))
    val (bestPosition, mostVisibleAsteroids) = mostVisibleAsteroidsFrom(input)
    println(mostVisibleAsteroids)
    println(laserOrder(input, bestPosition)(199)._3.intVal)
  }

  def mostVisibleAsteroidsFrom(matrix: Matrix): (Position, Int) = {
    val asteroids = matrix.asteroidPositions
    val (minInvisibleAsteroids, minPosition) = asteroids.map { candidate =>
      val invisibleAsteroids: Int =
        asteroids.filterNot(_ == candidate).count { visible =>
          matrix.findAsteroidsOnLine(candidate, visible).length > 2
        }
      (invisibleAsteroids, candidate)
    }.minBy(_._1)
    (minPosition, asteroids.length - minInvisibleAsteroids - 1)
  }

  def laserOrder(matrix: Matrix, laser: Position): Seq[(Int, Double, Position)] = {
    val asteroids = matrix.asteroidPositions
    asteroids.filterNot(_ == laser).map { asteroid =>
      val collisions = matrix.findAsteroidsOnLine(laser, asteroid).length
      val rad = rotation(laser, asteroid)
      (collisions, rad, asteroid)
    }.sortBy(p => (p._1, p._2))
  }

  def distance(a: Position, b: Position): Double = {
    val dx = (b.x - a.x).toDouble
    val dy = (b.y - a.y).toDouble
    Math.sqrt(dx * dx + dy * dy)
  }

  def rotation(laser: Position, asteroid: Position): Double = {
    val dx = laser.x - asteroid.x
    val dy = laser.y - asteroid.y
    val rad = - Math.atan2(dx, dy)
    if (rad < 0) rad + 2 * Math.PI else rad
  }

  trait Entity

  case object Asteroid extends Entity

  case object Space extends Entity

  case class Position(x: Int, y: Int) {
    def intVal: Int = x * 100 + y
  }

  private val epsilon: Double = 0.000001

  class Matrix(val mat: Vector[Vector[Entity]]) {
    def findAsteroidsOnLine(candidate: Position, visible: Position): Seq[Position] = {
      val lineDistance = distance(candidate, visible)
      asteroidPositions.filter { current =>
        distance(candidate, current) + distance(current, visible) - lineDistance < epsilon
      }
    }

    val asteroidPositions: Seq[Position] = {
      mat.zipWithIndex.flatMap {
        case (row, y) =>
          row.zipWithIndex.flatMap {
            case (Asteroid, x) => Some(Position(x, y))
            case (Space, _) => None
          }
      }
    }
  }

  object Matrix {
    def apply(strings: Seq[String]): Matrix = new Matrix(strings.map(_.toVector.map {
      case '#' => Asteroid
      case '.' => Space
    }).toVector)
  }

}
