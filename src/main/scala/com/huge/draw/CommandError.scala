package com.huge.draw

sealed trait CommandError extends Throwable
class InvalidArguments(msj: String) extends CommandError