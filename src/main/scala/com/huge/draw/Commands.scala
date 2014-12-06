package com.huge.draw

sealed trait Command

case class CreateCanvasCommand(width: Int, height: Int) extends Command
object CreateCanvasCommand {
  def unapply(args: List[String]) =
    if (args.isEmpty || args.head != "C") None
    else args.tail match {
        case width :: height :: Nil => {
          try {
            Some(width.toInt, height.toInt)
          } catch {
            case _: NumberFormatException => 
              throw new InvalidArguments(s"Width (${width}) and height (${height}) must be integers")
          }
        }
        case _ => throw new InvalidArguments("Width and height need to be specified: C 10 10")
      }
}