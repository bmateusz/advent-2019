package hu.bmateusz

object FileOperations {

  def readResource(resource: String): String = scala.io.Source.fromResource(resource).mkString

  def readResourceLines(resource: String): Seq[String] = scala.io.Source.fromResource(resource).getLines.toSeq

  def lineToInts(str: String): Seq[Int] = str.split("\\s").map(_.toInt)

  def readResourceIntLine(resource: String): Seq[Int] = scala.io.Source.fromResource(resource).mkString.split(',').map(_.toInt)

}
