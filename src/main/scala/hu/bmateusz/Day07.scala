package hu.bmateusz

import Intcode._

import scala.annotation.tailrec

object Day07 {

  /** https://adventofcode.com/2019/day/7 */
  def main(args: Array[String]): Unit = {
    val input = ProgramMemory(FileOperations.readResourceIntLine("day07.txt"))
    println(largestOutputSignal(input))
    println(largestOutputSignalFeedbackLoop(input))
  }

  def largestOutputSignal(program: ProgramMemory): BigInt = {
    (0 until 5).permutations.map {
      _.foldLeft(BigInt(0)) {
        case (acc, currPerm) =>
          runIntcode(program, Seq(currPerm, acc)).output.last
      }
    }.max
  }

  def largestOutputSignalFeedbackLoop(program: ProgramMemory): BigInt = {
    (5 until 10).permutations.map { perm =>
      val init = (Seq[BigInt](perm.head, 0) +: perm.tail.map(Seq[BigInt](_)).toList).map {
        input => Result(program, input, Seq.empty, 0, 0)
      }
      runWithFeedbackLoop(init, 0)
    }.max
  }

  @tailrec
  def runWithFeedbackLoop(programs: List[Result], index: Int): BigInt = {
    val runProgram = programs(index).run
    val nextIndex = (index + 1) % programs.size
    if (nextIndex == 0 && runProgram.isStopped) {
      runProgram.output.last
    } else {
      runWithFeedbackLoop(
        programs
          .updated(index, runProgram.copy(output = Seq.empty))
          .updated(nextIndex, programs(nextIndex).copy(input = programs(nextIndex).input ++ runProgram.output)),
        nextIndex
      )
    }
  }

}
