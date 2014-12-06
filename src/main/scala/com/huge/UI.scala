package com.huge

trait IO {
  def print(message: String) = Console.print(message)
  def println(message: String) = Console.println(message)
  val commands: Iterator[String]
}

trait StdIO extends IO {
  val commands = io.Source.stdin.getLines
}

abstract class UI extends IO {
  def run {
    print("Welcome; enter command: ")
    for (command <- commands) {
      if (command == "Q") {print("program finished"); return}
      else process(command)
    }
  }

  def process(command: String) = {
    print("enter command: ")
  }
}