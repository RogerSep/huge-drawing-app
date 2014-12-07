package com.huge.draw

/**
 * A representation of a command that can be applied over a canvas.
 */
sealed trait Command {
  /**
   * Transforms a given canvas into a new one after applying a transformation.
   * Implementors should override this function for implementing their behaviour.
   * @param canvas the canvas to transform
   * @return the transformed canvas
   */
  protected def transform(canvas: Canvas): Canvas
  def apply(canvas: Canvas): Canvas = {
    if (canvas.isEmpty) throw new UnsupportedCommand("The canvas is empty")
    else transform(canvas)
  }
}

/**
 * A point helper class
 * @param x the x coordinate
 * @param y the y coordinate
 */
case class Point(x: Int, y: Int) {
  override def toString = s"($x, $y)"

  /**
   * Calculates the distance between two points
   * @param point
   * @return the distance between this point and the point provided
   */
  def distance(point: Point): Double = math.hypot((point.x - this.x), (point.y - this.y))
}

/**
 * A command to create a canvas. Discards a previous canvas.
 * Perhaps it can resize the previous canvas?
 * @param width the width of the new canvas.
 * @param height the height of the new canvas.
 */
case class CreateCanvasCommand(width: Int, height: Int) extends Command {
  if (width <= 0 || height <= 0) 
    throw new InvalidArguments(s"Width (${width}) and height (${height}) must be positive non-zero integers")

  protected def transform(canvas: Canvas) =
    Canvas(Array.tabulate(height, width)((_, _) => ' '))
  
  override def apply(canvas: Canvas): Canvas = transform(canvas)
}

/**
 * Command to draw a Line inside a canvas.
 * @param x1 the x coordinate of the first point
 * @param y1 the y coordinate of the first point
 * @param x2 the x coordinate of the second point
 * @param y2 the y coordinate of the second point
 */
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
      throw new OutOfBounds(s"The point ${if (!canvas.contains(p1)) p1 else p2} is out of the canvas.")
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

/**
 * Composed out of 4 line commands creates a rectangle in the canvas.
 * @param x1
 * @param y1
 * @param x2
 * @param y2
 */
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

case class BucketFillCommand(x: Int, y: Int, colour: Char) extends Command {
  import scala.collection.mutable.Stack

  private val point = Point(x, y)

  /**
   * Determines the poinst that need to be changed in the given canvas.
   * @param canvas
   * @return
   */
  private def pointsToChange(canvas: Canvas) = {
    val originalColor = canvas.get(point)
    var points: List[Point] = List()
    val stack: Stack[Point] = Stack(point)
    
    while (!stack.isEmpty) {
      val p = stack.pop
      if (
        canvas.contains(p) && 
        canvas.get(p) == originalColor && 
        !points.contains(p)
      ) {
        points = p :: points

        stack.push(p.copy(x = p.x + 1))
        stack.push(p.copy(x = p.x - 1))
        stack.push(p.copy(y = p.y + 1))
        stack.push(p.copy(y = p.y - 1))
      }
    }

    points
  }

  /**
   * Uses the flood fill algorithm to transform the given canvas.
   * @param canvas the canvas to transform
   * @return the transformed canvas
   */
  def transform(canvas: Canvas) = {
    if (!canvas.contains(point))
      throw new OutOfBounds(s"The point $point is not within the canvas")

    val points = pointsToChange(canvas)

    canvas.map { (x, y, c) =>
      if (points.contains(Point(x, y))) colour
      else c
    }
  }
}