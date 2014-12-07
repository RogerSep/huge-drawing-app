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

object BucketFillCommandExtractor {
  def unapply(args: List[String]): Option[BucketFillCommand] =
    if (args.isEmpty || args.head != "B") None
    else args.tail match {
      case x :: y :: xs => {
        try {
          val colour = xs match {
            case Nil => ' '
            case c :: Nil => 
              if (c.length == 1) c.charAt(0) 
              else throw new UnsupportedCommand("Can only paint with one character long colour")
            case _ => throw new InvalidArguments("Too many argumets")
          }

          Some(BucketFillCommand(x.toInt, y.toInt, colour))
        } catch {
          case _: NumberFormatException => throw new InvalidArguments("Coordinates must be integers")
        }
      }
      case _ => throw new InvalidArguments("A coordinate must be specified followed by an optional colour; whitespace is assumed when no colour is specified")
    }
}

object CommandExtractor {
  def unapply(args: List[String]): Option[Command] = {
    args match {
      case CreateCanvasExtractor(c) => Some(c)
      case LineCommandExtractor(l) => Some(l)
      case RectangleCommandExtractor(r) => Some(r)
      case BucketFillCommandExtractor(b) => Some(b)
      case _ => None
    }
  }
}