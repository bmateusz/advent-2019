package hu.bmateusz

object Day03 {

  /** https://adventofcode.com/2019/day/3 */
  def main(args: Array[String]): Unit = {
    val input = FileOperations.readResourceCommaSeparatedLines("day03.txt").map(_.map(turtleFromString))
    val positions = input.map(_.foldLeft(Vector(startingPosition)) {
      case (positions: Vector[Position], turtle: Turtle) =>
        positions :+ positions.last.move(turtle)
    })
    val lines = positions.map(p => (p.zip(p.tail)).map { case (a, b) => Line(a, b) })
    println(findClosest(findIntersections(lines)).map(_.manhattanDistance))
    println(findBest(lines).headOption)
  }

  def findIntersections(lines: Seq[Vector[Line]]): Seq[Position] = {
    val first: Seq[Line] = lines(0)
    val second: Seq[Line] = lines(1)
    first.flatMap { a =>
      second.flatMap { b =>
        a.intersect(b)
      }
    }
  }

  def findBest(lines: Seq[Vector[Line]]): Seq[Int] = {
    findIntersections(lines).map(goal => stepToGoal(lines, goal) + stepToGoal(lines.reverse, goal)).filter(_ > 0).sorted
  }

  def stepToGoal(lines: Seq[Vector[Line]], goal: Position): Int = {
    val linesToGoal = lines.head.takeWhile(x => !x.contains(goal))
    linesToGoal.map(_.vector.manhattanDistance).sum +
      linesToGoal.lastOption.map(lastLine => Line(lastLine.to, goal).vector.manhattanDistance).getOrElse(0)
  }

  def findClosest(positions: Seq[Position]): Option[Position] = {
    positions.filterNot(_ == startingPosition).sortBy(p => p.manhattanDistance).headOption
  }

  trait Turtle {
    val move: Int

    def movePosition(from: Position): Position
  }

  case class Up(move: Int) extends Turtle {
    override def movePosition(from: Position): Position = from.subY(move)
  }

  case class Down(move: Int) extends Turtle {
    override def movePosition(from: Position): Position = from.addY(move)
  }

  case class Left(move: Int) extends Turtle {
    override def movePosition(from: Position): Position = from.subX(move)
  }

  case class Right(move: Int) extends Turtle {
    override def movePosition(from: Position): Position = from.addX(move)
  }

  def turtleFromString(string: String): Turtle = {
    val move = string.substring(1).toInt
    string.charAt(0) match {
      case 'U' => Up(move)
      case 'D' => Down(move)
      case 'L' => Left(move)
      case 'R' => Right(move)
    }
  }

  case class Position(x: Int, y: Int) {
    val manhattanDistance: Int = Math.abs(x) + Math.abs(y)

    def addX(int: Int): Position = Position(x + int, y)

    def addY(int: Int): Position = Position(x, y + int)

    def subX(int: Int): Position = Position(x - int, y)

    def subY(int: Int): Position = Position(x, y - int)

    def move(turtle: Turtle): Position = turtle.movePosition(this)
  }

  val startingPosition: Position = Position(0, 0)

  case class Line(from: Position, to: Position) {
    val vector: Position = Position(to.x - from.x, to.y - from.y)

    def contains(position: Position): Boolean = {
      ((from.x <= position.x && position.x <= to.x) ||
        (to.x <= position.x && position.x <= from.x)) &&
        ((from.y <= position.y && position.y <= to.y) ||
          (to.y <= position.y && position.y <= from.y))
    }

    def intersect(other: Line): Option[Position] = {
      val possibleIntersection = Position(
        if (vector.x == 0) from.x else other.from.x,
        if (vector.y == 0) from.y else other.from.y
      )
      if (contains(possibleIntersection) && other.contains(possibleIntersection)) {
        Some(possibleIntersection)
      } else {
        None
      }
    }

    def distanceToPosition(goal: Position): Int = {
      Line(from, goal).vector.manhattanDistance
    }

  }

}
