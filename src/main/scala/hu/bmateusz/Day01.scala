package hu.bmateusz

object Day01 {

  /** https://adventofcode.com/2019/day/1 */
  def main(args: Array[String]): Unit = {
    val input = FileOperations
      .readResourceLines("day01.txt")
      .map(_.toInt)
    println(taskOne(input))
    println(taskTwo(input))
  }

  def taskOne(input: Seq[Int]): Int = {
    input
      .map(_ / 3 - 2)
      .sum
  }

  def taskTwo(input: Seq[Int]): Int = {
    input
      .map(_ / 3 - 2)
      .map(moreFuel)
      .sum
  }

  def moreFuel(fuelMass: Int): Int = {
    fuelMass + List.unfold(fuelMass) { mass =>
      val newMass = mass / 3 - 2
      if (newMass > 0) {
        Some((newMass, newMass))
      } else {
        None
      }
    }.sum
  }
}
