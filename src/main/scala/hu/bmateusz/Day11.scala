package hu.bmateusz

import hu.bmateusz.Intcode._

import scala.annotation.tailrec

object Day11 {

  /** https://adventofcode.com/2019/day/11 */
  def main(args: Array[String]): Unit = {
    val input = ProgramMemory.fromString(FileOperations.readResourceLines("day11.txt").head).toIntcodeState
    println(runRobot(input, InitialBlack))
    println(runRobot(input, InitialWhite))
  }

  sealed trait Direction {
    val dx: Int
    val dy: Int
    val right: Direction
    val left: Direction
  }

  case object Up extends Direction {
    override val dx: Int = 0
    override val dy: Int = -1
    override val right: Direction = Right
    override val left: Direction = Left
  }

  case object Right extends Direction {
    override val dx: Int = 1
    override val dy: Int = 0
    override val right: Direction = Down
    override val left: Direction = Up
  }

  case object Down extends Direction {
    override val dx: Int = 0
    override val dy: Int = 1
    override val right: Direction = Left
    override val left: Direction = Right
  }

  case object Left extends Direction {
    override val dx: Int = -1
    override val dy: Int = 0
    override val right: Direction = Up
    override val left: Direction = Down
  }

  class Turtle(val direction: Direction,
               val position: Position) {
    def rotateAndStep(int: Int): Turtle = {
      val nextDirection = int match {
        case 0 => direction.left
        case 1 => direction.right
      }
      new Turtle(nextDirection, position.increase(nextDirection.dx, nextDirection.dy))
    }

    override def toString: String = s"Turtle($direction, $position)"
  }

  sealed trait Color {
    def asInt: Int
  }

  case object Black extends Color {
    override def asInt: Int = 0
    override def toString: String = "▁"
  }

  case object White extends Color {
    override def asInt: Int = 1

    override def toString: String = "▓"
  }

  case object InitialBlack extends Color {
    override def asInt: Int = 0

    override def toString: String = " "
  }

  case object InitialWhite extends Color {
    override def asInt: Int = 1

    override def toString: String = "S"
  }

  def colorFromInt(int: Int): Color = {
    int match {
      case 0 => Black
      case 1 => White
    }
  }

  class RobotState(val matrix: Matrix[Color],
                   val program: IntcodeState,
                   val turtle: Turtle) {
    def step(): RobotState = {
      val input = matrix.get(turtle.position)
      val result = program.copy(input = Seq(input.asInt)).run
      val paint = result.output(0).toInt
      val rotation = result.output(1).toInt
      val newMatrix = matrix.set(turtle.position, colorFromInt(paint))
      val newTurtle = turtle.rotateAndStep(rotation)
      new RobotState(newMatrix, result.copy(output = Seq.empty), newTurtle)
    }

    def countPaintedPanels: Int = matrix.flatten.count {
      case Black | White => true
      case InitialBlack | InitialWhite => false
    }

    override def toString: String = {
      countPaintedPanels + "\n" + matrix.trim(InitialBlack)
    }
  }

  def runRobot(input: IntcodeState, startingColor: Color): RobotState = {
    val size = 140
    val initialPosition = new Position(size / 2, size / 2)
    val initialMatrix = Matrix.empty[Color](size, InitialBlack).set(initialPosition, startingColor)
    val initialState = new RobotState(initialMatrix, input, new Turtle(Up, initialPosition))
    runRobotState(initialState)
  }

  @tailrec
  def runRobotState(robotState: RobotState): RobotState = {
    val next = robotState.step()
    if (next.program.isStopped) next else runRobotState(next)
  }

}
