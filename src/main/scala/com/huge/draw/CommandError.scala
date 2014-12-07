package com.huge.draw

/**
 * The errors that might throw the operations with the drawing interface.
 */
sealed trait CommandError extends Throwable {
  val msj: String
}
class InvalidArguments(val msj: String) extends CommandError
class UnsupportedCommand(val msj: String) extends CommandError
class OutOfBounds(val msj: String) extends CommandError