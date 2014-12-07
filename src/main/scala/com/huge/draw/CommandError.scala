package com.huge.draw

sealed trait CommandError extends Throwable
class InvalidArguments(msj: String) extends CommandError
class UnsupportedCommand(msj: String) extends CommandError
class OutOfBounds(msj: String) extends CommandError