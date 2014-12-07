package com.huge.draw

case class Canvas (layout: Array[Array[Char]] = Array()) {
  val width = layout match {
    case Array() => 0
    case Array(head, _*) => head.length
  }
  def apply(command: Command): Canvas = command(this)
  override def toString = {
    val sb = new StringBuffer
    val horizontalMargin = "-" * (width + (if (width > 0) 2 else 0))
    sb.append(horizontalMargin + "\n")
    layout.foreach(row => sb.append("|" + row.mkString("") + "|\n"))
    sb.append(horizontalMargin)

    sb.toString
  }
}