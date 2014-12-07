package com.huge.draw

sealed trait CommandError extends Throwable {
  val msj: String
}
class InvalidArguments(val msj: String) extends CommandError
class UnsupportedCommand(val msj: String) extends CommandError
class OutOfBounds(val msj: String) extends CommandError