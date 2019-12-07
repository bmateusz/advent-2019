package hu.bmateusz

import scala.annotation.tailrec

object Intcode {

  case class Result(program: Seq[Int], input: Seq[Int], output: Seq[Int], pointer: Int) {
    def isStopped: Boolean = program(pointer) == 99

    def run: Result = runIntcode(program, input, output, pointer)
  }

  @tailrec
  def runIntcode(program: Seq[Int], input: Seq[Int] = Seq.empty, output: Seq[Int] = Seq.empty, pointer: Int = 0): Result = {
    val longOp = program(pointer).toString.map(_.asDigit).reverse.padTo(5, 0).reverse
    val p1 = parameterMode(longOp(2), program) _
    val p2 = parameterMode(longOp(1), program) _
    // val p3 = parameterMode(longOp(0), program) _
    val op = longOp(3) * 10 + longOp(4)
    op match {
      case 1 | 2 =>
        val res = if (op == 1) p1(pointer + 1) + p2(pointer + 2) else p1(pointer + 1) * p2(pointer + 2)
        runIntcode(
          program.updated(program(pointer + 3), res),
          input,
          output,
          pointer + 4,
        )
      case 3 =>
        if (input.isEmpty) {
          Result(program, input, output, pointer)
        } else {
          runIntcode(
            program.updated(program(pointer + 1), input.head),
            input.tail,
            output,
            pointer + 2
          )
        }
      case 4 =>
        runIntcode(
          program,
          input,
          output :+ program(program(pointer + 1)),
          pointer + 2
        )
      case 5 | 6 =>
        if ((op == 5 && p1(pointer + 1) != 0) || (op == 6 && p1(pointer + 1) == 0)) {
          runIntcode(
            program,
            input,
            output,
            p2(pointer + 2)
          )
        } else {
          runIntcode(
            program,
            input,
            output,
            pointer + 3
          )
        }
      case 7 | 8 =>
        val res = (op == 7 && p1(pointer + 1) < p2(pointer + 2)) || (op == 8 && p1(pointer + 1) == p2(pointer + 2))
        runIntcode(
          program.updated(program(pointer + 3), boolToInt(res)),
          input,
          output,
          pointer + 4,
        )
      case 99 =>
        Result(program, input, output, pointer)
      case op: Int =>
        println(program)
        println(s"ERROR $op at $pointer")
        Result(Seq.empty, input, output, pointer)
    }
  }

  private def parameterMode(mode: Int, program: Seq[Int])(p: Int): Int = {
    mode match {
      case 0 => program(program(p))
      case 1 => program(p)
    }
  }

  private def boolToInt(bool: Boolean): Int = if (bool) 1 else 0

}
