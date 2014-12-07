package com.huge.draw

sealed trait Command {
  protected def transform(canvas: Canvas): Canvas
  def apply(canvas: Canvas): Canvas = {
    if (canvas.layout == Array()) canvas
    else transform(canvas)
  }
}

case class CreateCanvasCommand(width: Int, height: Int) extends Command {
  if (width <= 0 || height <= 0) 
    throw new InvalidArguments(s"Width (${width}) and height (${height}) must be positive non-zero integers")

  protected def transform(canvas: Canvas) = Canvas((1 to height).toArray.map(_ => (" " * width).split("").map(_.charAt(0))))
  override def apply(canvas: Canvas): Canvas = transform(canvas)
}

object CreateCanvasCommand {
  def unapply(args: List[String]): Option[CreateCanvasCommand] =
    if (args.isEmpty || args.head != "C") None
    else args.tail match {
        case width :: height :: Nil => {
          try {
            Some(CreateCanvasCommand(width.toInt, height.toInt))
          } catch {
            case _: NumberFormatException => 
              throw new InvalidArguments(s"Width (${width}) and height (${height}) must be integers")
          }
        }
        case _ => throw new InvalidArguments("Width and height need to be specified")
      }
}