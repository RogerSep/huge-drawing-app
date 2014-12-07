package com.huge

import com.huge.draw._

trait IO {
  protected def print(message: String) = Console.print(message)
  protected def println(message: String) = Console.println(message)
  protected val commands: Iterator[String]
}

trait StdIO extends IO {
  protected val commands = io.Source.stdin.getLines
}

abstract class UI extends IO {
  private var canvas = Canvas()

  def run {
    print("Welcome; enter command: ")
    for (command <- commands) {
      if (command == "Q") {print("program finished"); return}
      else process(command)
    }
  }

  def process(command: String) = {
    try {
      canvas = format(command) match {
        case CommandExtractor(command) => canvas(command)
        case _ => {
          println("Incorrect command; try again: ")
          canvas
        }
      }
    } catch {
      case e: CommandError => println(e.msj)
    }

    println("\n" + canvas.toString)

    print("enter command: ")
  }

  def format(rawCommand: String): List[String] = {
    rawCommand
      .trim
      .replaceAll("\\s+", " ")
      .split(" ")
      .toList
  }
}

class UIFromArguments(comm: Array[String]) extends UI {
  protected val commands = comm.toIterator
}