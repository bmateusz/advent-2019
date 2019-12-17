package hu.bmateusz

import hu.bmateusz.Intcode._

import scala.annotation.tailrec

object Day15 {

  /** https://adventofcode.com/2019/day/15 */
  def main(args: Array[String]): Unit = {
    val input = ProgramMemory.fromString(FileOperations.readResourceLines("day15.txt").head).toIntcodeState
    val result = walk(input)
    println(fill(result))
  }

  val north = 1
  val south = 2
  val west = 3
  val east = 4
  val directions = List(north, south, west, east)

  val wall = 0
  val moved = 1
  val oxygen = 2
  val undiscovered = 5

  def move(position: Position, direction: Int): Position = {
    direction match {
      case 1 => position.increase(0, 1)
      case 2 => position.increase(0, -1)
      case 3 => position.increase(-1, 0)
      case 4 => position.increase(1, 0)
    }
  }

  def walk(input: IntcodeState,
           map: UnboundedMatrix[Int] = UnboundedMatrix.empty[Int](undiscovered),
           position: Position = Position(0, 0),
           numberOfSteps: Int = 0): UnboundedMatrix[Int] = {
    directions.foldLeft(map) { case (currMap, direction) =>
      val newPosition = move(position, direction)
      if (currMap.get(newPosition) == undiscovered) {
        val step = input.in(direction).run
        val stepResult = step.output.head.toInt
        val newMap = currMap.set(newPosition, stepResult)

        if (stepResult == oxygen) {
          println(s"${numberOfSteps + 1} $newPosition")
          newMap
        } else if (stepResult == moved) {
          walk(step.flush, newMap, newPosition, numberOfSteps + 1)
        } else if (stepResult == wall) {
          newMap
        } else {
          newMap
        }
      } else {
        currMap // step back
      }
    }
  }

  @tailrec
  def fill(map: UnboundedMatrix[Int] = UnboundedMatrix.empty[Int](undiscovered),
           numberOfSteps: Int = 0): Int = {
    if (map.mat.exists(_._2 == moved)) {
      val newMap = map.mat.filter(_._2 == oxygen).keys.flatMap { pos =>
        directions.map { dir =>
          move(pos, dir)
        }
      }.foldLeft(map) {
        case (currMap, pos) =>
          if (currMap.get(pos) == moved) {
            currMap.set(pos, oxygen)
          } else {
            currMap
          }
      }
      fill(newMap, numberOfSteps + 1)
    } else {
      numberOfSteps
    }
  }

}
