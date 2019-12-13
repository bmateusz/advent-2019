package hu.bmateusz

import scala.annotation.tailrec

object Intcode {

  case class IntcodeState(program: ProgramMemory, input: Seq[BigInt], output: Seq[BigInt], pointer: BigInt, relativeBase: BigInt) {
    def in(in: BigInt): IntcodeState = copy(input = Seq(in))

    def in(in: Seq[BigInt]): IntcodeState = copy(input = in)

    def flush: IntcodeState = copy(output = Seq.empty)

    def isStopped: Boolean = program.get(pointer) == 99

    def run: IntcodeState = runIntcode(program, input, output, pointer, relativeBase)

    def updatedProgram(idx: Int, value: BigInt): IntcodeState = copy(program = program.updated(idx, value))
  }

  class ProgramMemory(val map: Map[BigInt, BigInt]) {
    def get(idx: BigInt): BigInt = {
      if (idx < 0) {
        throw new IndexOutOfBoundsException(s"Negative index $idx")
      } else {
        map.getOrElse(idx, 0)
      }
    }

    def updated(idx: BigInt, value: BigInt): ProgramMemory = new ProgramMemory(map.updated(idx, value))

    def head: BigInt = get(0)

    override def toString: String = map.toList.sortBy(_._1).toString()

    def toIntcodeState: IntcodeState = IntcodeState(this, Seq.empty, Seq.empty, 0, 0)
  }

  object ProgramMemory {
    def apply(seq: Seq[Int]) = new ProgramMemory(seq.zipWithIndex.map(p => BigInt(p._2) -> BigInt(p._1)).toMap)

    def fromBigInts(seq: Seq[BigInt]) = new ProgramMemory(seq.zipWithIndex.map(p => BigInt(p._2) -> p._1).toMap)

    def fromString(seq: String): ProgramMemory = fromBigInts(seq.split(',').map(BigInt(_)))
  }

  @tailrec
  def runIntcode(program: ProgramMemory,
                 input: Seq[BigInt] = Seq.empty,
                 output: Seq[BigInt] = Seq.empty,
                 pointer: BigInt = 0,
                 relativeBase: BigInt = 0): IntcodeState = {
    val longOp = program.get(pointer).toString.map(_.asDigit).reverse.padTo(5, 0).reverse
    lazy val p1 = parameterMode(longOp(2), program, relativeBase, pointer + 1)
    lazy val p2 = parameterMode(longOp(1), program, relativeBase, pointer + 2)
    lazy val p3 = parameterMode(longOp(0), program, relativeBase, pointer + 3)
    val op = longOp(3) * 10 + longOp(4)
    // println(s"$pointer @ $longOp $op | $relativeBase")
    op match {
      case 1 | 2 =>
        val res = if (op == 1) program.get(p1) + program.get(p2) else program.get(p1) * program.get(p2)
        runIntcode(
          program.updated(p3, res),
          input,
          output,
          pointer + 4,
          relativeBase,
        )
      case 3 =>
        if (input.isEmpty) {
          IntcodeState(program, input, output, pointer, relativeBase)
        } else {
          runIntcode(
            program.updated(p1, input.head),
            input.tail,
            output,
            pointer + 2,
            relativeBase,
          )
        }
      case 4 =>
        runIntcode(
          program,
          input,
          output :+ program.get(p1),
          pointer + 2,
          relativeBase,
        )
      case 5 | 6 =>
        val jumpTo = if ((op == 5 && program.get(p1) != 0) || (op == 6 && program.get(p1) == 0)) {
          program.get(p2)
        } else {
          pointer + 3
        }
        runIntcode(
          program,
          input,
          output,
          jumpTo,
          relativeBase,
        )
      case 7 | 8 =>
        val res = (op == 7 && program.get(p1) < program.get(p2)) || (op == 8 && program.get(p1) == program.get(p2))
        runIntcode(
          program.updated(p3, boolToInt(res)),
          input,
          output,
          pointer + 4,
          relativeBase,
        )
      case 9 =>
        runIntcode(
          program,
          input,
          output,
          pointer + 2,
          relativeBase + program.get(p1),
        )
      case 99 =>
        IntcodeState(program, input, output, pointer, relativeBase)
      case op: Int =>
        println(program)
        println(s"ERROR $op at $pointer")
        IntcodeState(program, input, output, pointer, relativeBase)
    }
  }

  private def parameterMode(mode: Int, program: ProgramMemory, relativeBase: BigInt, p: BigInt): BigInt = {
    mode match {
      case 0 => program.get(p) // position mode
      case 1 => p // immediate mode
      case 2 => relativeBase + program.get(p) // relative
    }
  }

  private def boolToInt(bool: Boolean): Int = if (bool) 1 else 0

  def main(args: Array[String]): Unit = {
    def assert(actual: BigInt, expected: BigInt): Unit = {
      if (actual == expected) println("ok") else throw new Exception(s"$actual != $expected")
    }

    println(s"Run integration tests")
    val input02 = ProgramMemory(FileOperations.readResourceIntLine("day02.txt"))
    assert(runIntcode(input02.updated(1, 12).updated(2, 2)).program.head, 7210630)
    assert(Day02.findFor(input02, 19690720).map(r => r._1 * 100 + r._2).get, 3892)

    val input05 = ProgramMemory(FileOperations.readResourceIntLine("day05.txt"))
    assert(runIntcode(input05, Seq(1)).output.last, 11933517)
    assert(runIntcode(input05, Seq(5)).output.last, 10428568)

    val input07 = ProgramMemory(FileOperations.readResourceIntLine("day07.txt"))
    assert(Day07.largestOutputSignal(input07), 199988)
    assert(Day07.largestOutputSignalFeedbackLoop(input07), 17519904)

    val input09 = ProgramMemory.fromString(FileOperations.readResourceLines("day09.txt").head)
    assert(runIntcode(input09, Seq(1)).output.last, 3780860499L)
    assert(runIntcode(input09, Seq(2)).output.last, 33343)
  }

}
