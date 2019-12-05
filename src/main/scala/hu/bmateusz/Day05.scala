package hu.bmateusz

import scala.annotation.tailrec

object Day05 {

  /** https://adventofcode.com/2019/day/5 */
  def main(args: Array[String]): Unit = {
    val input = FileOperations.readResourceIntLine("day05.txt")
    println(parseIntcode(input, Seq(1)).output.last)
    println(parseIntcode(input, Seq(5)).output.last)
  }

  def parameterMode(mode: Int, program: Seq[Int])(p: Int): Int = {
    mode match {
      case 0 => program(program(p))
      case 1 => program(p)
    }
  }

  case class Result(program: Seq[Int], input: Seq[Int], output: Seq[Int])

  def boolToInt(bool: Boolean): Int = if (bool) 1 else 0

  @tailrec
  def parseIntcode(program: Seq[Int], input: Seq[Int] = Seq.empty, output: Seq[Int] = Seq.empty, pointer: Int = 0): Result = {
    val longOp = program(pointer).toString.map(_.toString.toInt).reverse.padTo(5, 0).reverse
    val p1 = parameterMode(longOp(2), program) _
    val p2 = parameterMode(longOp(1), program) _
    // val p3 = parameterMode(longOp(0), program) _
    val op = longOp(3) * 10 + longOp(4)
    op match {
      case 1 | 2 =>
        val res = if (op == 1) p1(pointer + 1) + p2(pointer + 2) else p1(pointer + 1) * p2(pointer + 2)
        parseIntcode(
          program.updated(program(pointer + 3), res),
          input,
          output,
          pointer + 4,
        )
      case 3 =>
        parseIntcode(
          program.updated(program(pointer + 1), input.head),
          input.tail,
          output,
          pointer + 2
        )
      case 4 =>
        parseIntcode(
          program,
          input,
          output :+ program(program(pointer + 1)),
          pointer + 2
        )
      case 5 | 6 =>
        if ((op == 5 && p1(pointer + 1) != 0) || (op == 6 && p1(pointer + 1) == 0)) {
          parseIntcode(
            program,
            input,
            output,
            p2(pointer + 2)
          )
        } else {
          parseIntcode(
            program,
            input,
            output,
            pointer + 3
          )
        }
      case 7 | 8 =>
        val res = (op == 7 && p1(pointer + 1) < p2(pointer + 2)) || (op == 8 && p1(pointer + 1) == p2(pointer + 2))
        parseIntcode(
          program.updated(program(pointer + 3), boolToInt(res)),
          input,
          output,
          pointer + 4,
        )
      case 99 =>
        Result(program, input, output)
      case op: Int =>
        println(s"ERROR $op at $pointer")
        println(program)
        Result(Seq.empty, input, output)
    }
  }

}
