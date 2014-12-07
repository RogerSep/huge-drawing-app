package com.huge.draw

sealed trait Command {
  protected def transform(canvas: Canvas): Canvas
  def apply(canvas: Canvas): Canvas = {
    if (canvas.isEmpty) canvas
    else transform(canvas)
  }
}

case class Point(x: Int, y: Int) {
  override def toString = s"($x, $y)"

  def distance(point: Point): Double = math.hypot((point.x - this.x), (point.y - this.y))
}

case class CreateCanvasCommand(width: Int, height: Int) extends Command {
  if (width <= 0 || height <= 0) 
    throw new InvalidArguments(s"Width (${width}) and height (${height}) must be positive non-zero integers")

  protected def transform(canvas: Canvas) =
    Canvas(Array.tabulate(height, width)((_, _) => ' '))
  
  override def apply(canvas: Canvas): Canvas = transform(canvas)
}

case class LineCommand(x1: Int, y1: Int, x2: Int, y2: Int) extends Command {
  if (x1 <= 0 || y1 <= 0 || x2 <= 0 || y2 <= 0)
    throw new InvalidArguments(s"Coordinates must be greater than 0 (($x1, $y1), ($x2, $y2))")

  val (horizontal, vertical) = (y1 == y2, x1 == x2)

  if (!horizontal && !vertical)
    throw new UnsupportedCommand("Only vertical or horizontal lines can be drawn")

  private val (p1, p2) = {
    val (p1, p2) = (Point(x1, y1), Point(x2, y2))
    val origin = Point(0, 0)
    
    if (p1.distance(origin) < p2.distance(origin)) (p1, p2)
    else (p2, p1)
  }

  protected def transform(canvas: Canvas) = 
    if (!canvas.contains(p1) || !canvas.contains(p2)) 
      throw new OutOfBounds("The point ${if (!canvas.contains(p1)) p1 else p2} is out of the canvas.")
    else drawLine(canvas)

  private def contains(x: Int, y: Int) =
    if (horizontal) p2.y == y && p1.x <= x && x <= p2.x
    else p2.x == x && p1.y <= y && y <= p2.y

  private def drawLine(canvas: Canvas) = {
    canvas.map { (x, y, c) => 
      if (this.contains(x, y)) 'x'
      else c
    }
  }
}

case class RectangleCommand(x1: Int, y1: Int, x2: Int, y2: Int) extends Command {
  if (x1 <= 0 || y1 <= 0 || x2 <= 0 || y2 <= 0)
    throw new InvalidArguments(s"Coordinates must be greater than 0 (($x1, $y1), ($x2, $y2))")

  private val edges = List(
    LineCommand(x1, y1, x2, y1),
    LineCommand(x1, y1, x1, y2),
    LineCommand(x1, y2, x2, y2),
    LineCommand(x2, y1, x2, y2)
  )

  protected def transform(canvas: Canvas) = {
    edges.foldRight(canvas) { (lineCommand, canvas) =>
      canvas.apply(lineCommand)
    }
  }
}