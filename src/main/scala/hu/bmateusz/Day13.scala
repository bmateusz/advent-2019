package hu.bmateusz

import hu.bmateusz.Intcode._

import scala.annotation.tailrec

object Day13 {

  /** https://adventofcode.com/2019/day/13 */
  def main(args: Array[String]): Unit = {
    val input = ProgramMemory.fromString(FileOperations.readResourceLines("day13.txt").head).toIntcodeState
    println(countBlocks(input.run.output))
    println(play(input.updatedProgram(0, 2)))
  }

  private val block = 2
  private val bat = 3
  private val ball = 4

  case class GameState(screen: Matrix[Int],
                       score: Int)

  def countBlocks(output: Seq[BigInt]): Int = output.grouped(3).count(_.last == block)

  def draw(output: Seq[BigInt], state: GameState): GameState = {
    output.map(_.toInt).grouped(3).foldLeft(state) {
      case (GameState(matrix, score), curr) if curr(0) == -1 =>
        GameState(matrix, curr(2))
      case (GameState(matrix, score), curr) =>
        GameState(matrix.set(Position(curr(0), curr(1)), curr(2)), score)
    }
  }

  def positionsToJoystick(ballPosition: Option[Position], batPosition: Option[Position]): Int = {
    (ballPosition, batPosition) match {
      case (Some(ballp: Position), Some(batp: Position)) =>
        if (ballp.x == batp.x) 0 else if (ballp.x > batp.x) 1 else -1
      case _ => throw new IllegalArgumentException(s"Bad arguments $ballPosition $batPosition")
    }
  }

  def trySteps(intcodeState: IntcodeState, state: GameState): Int = {
    List(-1, 0, 1).minBy { candidate =>
      val tryNext = intcodeState.in(candidate).run
      val tryDrawn = draw(tryNext.output, state)
      val ballPos = tryDrawn.screen.find(ball).get
      val batPos = tryDrawn.screen.find(bat).get
      val closeToBat = Math.abs(ballPos.x - batPos.x)
      val gameLost = ballPos.y >= batPos.y
      if (gameLost) 100 else closeToBat
    }
  }

  @tailrec
  def play(intcodeState: IntcodeState, state: GameState = GameState(Matrix.empty(43, 23, 0), -1)): Int = {
    val joystick = trySteps(intcodeState, state)
    val next = intcodeState.in(joystick).run
    val drawn = draw(next.output, state)
    if (next.isStopped) {
      drawn.score
    } else {
      play(next.flush, drawn)
    }
  }

}
