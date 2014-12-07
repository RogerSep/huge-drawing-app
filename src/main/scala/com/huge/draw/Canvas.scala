package com.huge.draw

case class Canvas (private val layout: Array[Array[Char]] = Array()) {
  val height = layout.length
  val width = layout match {
    case Array() => 0
    case Array(head, _*) => head.length
  }
  
  def contains(point: Point) = 0 < point.x && point.x <= width && 0 < point.y && point.y <= height

  def apply(command: Command): Canvas = command(this)

  def map(f: (Int, Int, Char) => Char): Canvas = {
    Canvas(Array.tabulate(height, width){ (x, y) =>
        f(y + 1, x + 1, layout(x)(y))
      })
  }

  def isEmpty = layout.isEmpty

  override def toString = {
    val sb = new StringBuffer
    val horizontalMargin = "-" * (width + (if (width > 0) 2 else 0))
    sb.append(horizontalMargin + "\n")
    layout.foreach(row => sb.append("|" + row.mkString("") + "|\n"))
    sb.append(horizontalMargin)

    sb.toString
  }
}