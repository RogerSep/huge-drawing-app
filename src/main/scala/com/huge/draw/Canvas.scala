package com.huge.draw

/**
 * An immutable implementation the canvas.
 * @param layout the matrix that composes the canvas.
 */
case class Canvas (private val layout: Array[Array[Char]] = Array()) {
  val height = layout.length
  val width = layout match {
    case Array() => 0
    case Array(head, _*) => head.length
  }

  def contains(point: Point) = 0 < point.x && point.x <= width && 0 < point.y && point.y <= height

  /**
   * Applies a command over this Canvas instance yielding a new canvas.
   * @param commands A set of commands to execute successively over this canvas
   * @return the resulting canvas
   */
  def apply(commands: Command *): Canvas = 
    commands.foldLeft(this) { (command, canvas) =>
      canvas.apply(command)
    }

  /**
   * Returns a new canvas after appling a function 'f' over every node of this canvas.
   * @param f a function to apply to every node of this canvas. It takes the coordinate X, Y and the current colour.
   * @return the resulting canvas
   */
  def map(f: (Int, Int, Char) => Char): Canvas = {
    Canvas(Array.tabulate(height, width){ (x, y) =>
        f(y + 1, x + 1, layout(x)(y))
      })
  }

  /**
   * Gets the colour of a certain point in the canvas, 1 based.
   * @param point The point to find its colour
   * @return the colour of the asked point.
   */
  def get(point: Point): Char = layout(point.y - 1)(point.x - 1)

  def isEmpty = layout.isEmpty

  /**
   *
   * @return an screen drawable representation of this canvas
   */
  override def toString = {
    val sb = new StringBuffer
    val horizontalMargin = "-" * (width + (if (width > 0) 2 else 0))
    sb.append(horizontalMargin + "\n")
    layout.foreach(row => sb.append("|" + row.mkString("") + "|\n"))
    sb.append(horizontalMargin)

    sb.toString
  }
}