package hu.bmateusz

import Intcode._

object Day02 {

  /** https://adventofcode.com/2019/day/2 */
  def main(args: Array[String]): Unit = {
    val input = FileOperations.readResourceIntLine("day02.txt")
    println(runIntcode(
      input
        .updated(1, 12)
        .updated(2, 2)
    ).program.head)
    println(findFor(input, 19690720))
  }

  def findFor(input: Seq[Int], desired: Int): Option[(Int, Int)] = {
    (0 to 99).flatMap(n => (0 to 99).map((n, _))).find {
      case (noun, verb) =>
        runIntcode(input.updated(1, noun).updated(2, verb)).program.head == desired
    }
  }

}
