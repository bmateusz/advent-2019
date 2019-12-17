package hu.bmateusz


case class Position(x: Int, y: Int) {
  def increase(dx: Int, dy: Int): Position = Position(x + dx, y + dy)
}
