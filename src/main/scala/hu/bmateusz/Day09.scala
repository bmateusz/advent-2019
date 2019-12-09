package hu.bmateusz

import Intcode._

object Day09 {

  /** https://adventofcode.com/2019/day/9 */
  def main(args: Array[String]): Unit = {
    val input = ProgramMemory.fromString(FileOperations.readResourceLines("day09.txt").head)
    println(runIntcode(input, Seq(1)).output)
    println(runIntcode(input, Seq(2)).output)
  }

}
