package hu.bmateusz

class Matrix[T](val mat: Vector[Vector[T]]) {
  def get(p: Position): T = mat(p.y)(p.x)

  def set(p: Position, elem: T): Matrix[T] = new Matrix(mat.updated(p.y, mat(p.y).updated(p.x, elem)))

  def width: Int = mat.headOption.map(_.size).getOrElse(0)

  def height: Int = mat.size

  def find(elem: T): Option[Position] = {
    mat.view.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.find { case (e, x) =>
        e == elem
      }.map { case (e, x) =>
        Position(x, y)
      }
    }.headOption
  }

  override def toString: String = {
    mat.map(_.mkString).mkString("\n")
  }

  def flatten: Vector[T] = mat.flatten

  def trim(elem: T): Matrix[T] = {
    val topAndBottom = mat
      .dropWhile(_.exists(_ != elem) == false).reverse
      .dropWhile(_.exists(_ != elem) == false).reverse
    val left = topAndBottom.map(_.indexWhere(_ != elem)).min
    val right = topAndBottom.map(_.reverse.indexWhere(_ != elem)).min
    new Matrix[T](topAndBottom.map(_.drop(left).dropRight(right)))
  }
}

object Matrix {
  def empty[T](size: Int, elem: T): Matrix[T] = {
    val row = Vector.fill(size)(elem)
    new Matrix(Vector.fill(size)(row))
  }

  def empty[T](width: Int, height: Int, elem: T): Matrix[T] = {
    val row = Vector.fill(width)(elem)
    new Matrix(Vector.fill(height)(row))
  }
}
