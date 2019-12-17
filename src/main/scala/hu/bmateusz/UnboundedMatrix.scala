package hu.bmateusz

class UnboundedMatrix[T](val mat: Map[Position, T], val default: T) {
  def get(p: Position): T = mat.getOrElse(p, default)

  def set(p: Position, elem: T): UnboundedMatrix[T] = new UnboundedMatrix(mat + (p -> elem), default)

  lazy val bounds: (Position, Position) = {
    mat.keys.foldLeft((Position(0, 0), Position(0, 0))) {
      case ((topLeft, bottomRight), curr) =>
        (
          Position(Math.min(curr.x, topLeft.x), Math.max(curr.y, topLeft.y)),
          Position(Math.max(curr.x, bottomRight.x), Math.min(curr.y, bottomRight.y))
        )

    }
  }
  override def toString: String = {
    (bounds._1.x to bounds._2.x).map { x =>
      (bounds._2.y to bounds._1.y).map { y =>
        get(Position(x, y))
      }.mkString
    }.mkString("\n")
  }
}

object UnboundedMatrix {
  def empty[T](elem: T): UnboundedMatrix[T] = {
    new UnboundedMatrix[T](Map.empty, elem)
  }
}
