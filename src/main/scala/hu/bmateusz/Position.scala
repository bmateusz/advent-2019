package hu.bmateusz


class Position(val x: Int, val y: Int) {
  def increase(dx: Int, dy: Int): Position = new Position(x + dx, y + dy)

  override def toString: String = s"($x, $y)"
}
