package hu.bmateusz

object FileOperations {

  def readResource(resource: String): String = scala.io.Source.fromResource(resource).mkString

  def readResourceLines(resource: String): Seq[String] = scala.io.Source.fromResource(resource).getLines.toSeq

  def lineToInts(str: String): Seq[Int] = str.split("\\s").map(_.toInt)

  def readResourceCommaSeparatedLine(resource: String): Seq[String] = scala.io.Source.fromResource(resource).mkString.split(',')

  def readResourceIntLine(resource: String): Seq[Int] = readResourceCommaSeparatedLine(resource).map(_.toInt)

  def readResourceCommaSeparatedLines(resource: String): Seq[Seq[String]] = scala.io.Source.fromResource(resource).getLines().map(_.split(',').toSeq).toSeq

}
