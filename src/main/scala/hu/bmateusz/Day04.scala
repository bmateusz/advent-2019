package hu.bmateusz

object Day04 {

  /** https://adventofcode.com/2019/day/4 */
  def main(args: Array[String]): Unit = {
    val input = (248345 to 746315)
    println(input.count(i => meetsCriteria(i, _ >= 2)))
    println(input.count(i => meetsCriteria(i, _ == 2)))
  }

  def meetsCriteria(int: Int, digitPairsFunction: Int => Boolean): Boolean = {
    val digits = int.toString.map(_.toInt)
    if (digits.size == 6) {
      val digitsIncreasing = digits.zip(digits.tail).forall { case (l, r) => l <= r }
      val digitsPairs = digits.groupMapReduce(identity)(_ => 1)(_ + _)
      digitsIncreasing && digitsPairs.values.exists(digitPairsFunction)
    } else {
      false
    }
  }

}
