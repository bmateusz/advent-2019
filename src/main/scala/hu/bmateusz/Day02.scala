package hu.bmateusz

import scala.annotation.tailrec

object Day02 {

  /** https://adventofcode.com/2019/day/2 */
  def main(args: Array[String]): Unit = {
    val input = FileOperations.readResourceIntLine("day02.txt")
    println(parseIntcode(
      input
        .updated(1, 12)
        .updated(2, 2)
    ).head)
    println(findFor(input, 19690720))
  }

  def findFor(input: Seq[Int], desired: Int): Option[(Int, Int)] = {
    (0 to 99).flatMap(n => (0 to 99).map((n, _))).find {
      case (noun, verb) =>
        parseIntcode(input.updated(1, noun).updated(2, verb)).head == desired
    }
  }


  @tailrec
  def parseIntcode(input: Seq[Int], pointer: Int = 0): Seq[Int] = {
    input(pointer) match {
      case 1 =>
        parseIntcode(
          input.updated(input(pointer + 3), input(input(pointer + 1)) + input(input(pointer + 2))),
          pointer + 4
        )
      case 2 =>
        parseIntcode(
          input.updated(input(pointer + 3), input(input(pointer + 1)) * input(input(pointer + 2))),
          pointer + 4
        )
      case 99 =>
        input
      case _ =>
        println(s"ERROR at $pointer")
        println(input)
        Seq.empty
    }
  }

}
