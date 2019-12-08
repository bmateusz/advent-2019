package hu.bmateusz

object Day08 {

  /** https://adventofcode.com/2019/day/8 */
  def main(args: Array[String]): Unit = {
    val width = 25
    val height = 6
    val input = FileOperations
      .readResourceLines("day08.txt")
      .head
      .map(_.asDigit)
      .grouped(width * height)
      .toList

    println(checkForElfs(input))
    println(draw(width, decode(input)))
  }

  def checkForElfs(input: List[IndexedSeq[Int]]): Int = {
    val row = input.minBy(_.count(_ == 0))
    row.count(_ == 1) * row.count(_ == 2)
  }

  def decode(input: List[IndexedSeq[Int]]): IndexedSeq[Int] = {
    input.foldLeft(IndexedSeq.fill(input.head.length)(2)) {
      case (acc, curr) =>
        (acc.zip(curr)).map {
          case (2, c: Int) => c
          case (c, _) => c
        }
    }
  }

  def draw(width: Int, result: IndexedSeq[Int]): String = {
    result.grouped(width).map(_.map {
      case 0 => '▓'
      case 1 => '▁'
      case 2 => ' '
    }.mkString).mkString("\n")
  }

}
