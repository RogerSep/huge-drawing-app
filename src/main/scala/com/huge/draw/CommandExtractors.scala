package com.huge.draw

object CreateCanvasExtractor {
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

object LineCommandExtractor {
  def unapply(args: List[String]): Option[LineCommand] = 
    if (args.isEmpty || args.head != "L") None
    else args.tail match {
      case x1 :: y1 :: x2 :: y2 :: Nil => {
        try {
          Some(LineCommand(x1.toInt, y1.toInt, x2.toInt, y2.toInt))
        } catch {
          case _: NumberFormatException => 
            throw new InvalidArguments(s"All coordinates must be integers, (($x1, $y1), ($x2, $y2))")
        }
      }
      case _ => throw new InvalidArguments("Two points must be specified; e.g. L 1 1 1 5")
    }
}

object RectangleCommandExtractor {
  def unapply(args: List[String]): Option[RectangleCommand] =
    if (args.isEmpty || args.head != "R") None
    else args.tail match {
      case x1 :: y1 :: x2 :: y2 :: Nil => {
        try {
          Some(RectangleCommand(x1.toInt, y1.toInt, x2.toInt, y2.toInt))
        } catch {
          case _: NumberFormatException =>
            throw new InvalidArguments(s"All coordintas must be integers, (($x1, $y1), ($x2, $y2))")
        }
      }
      case _ => throw new InvalidArguments("Two points must be specified; e.g. R 1 1 4 4")
    }
}