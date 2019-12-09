package hu.bmateusz

import Intcode._

object Day05 {

  /** https://adventofcode.com/2019/day/5 */
  def main(args: Array[String]): Unit = {
    val input = ProgramMemory(FileOperations.readResourceIntLine("day05.txt"))
    println(runIntcode(input, Seq(1)).output.last)
    println(runIntcode(input, Seq(5)).output.last)
  }

}
