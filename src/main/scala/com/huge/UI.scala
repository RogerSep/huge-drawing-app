package com.huge

import com.huge.draw._

trait IO {
  protected def print(message: String) = Console.print(message)
  protected def println(message: String) = Console.println(message)
  protected val commands: Iterator[String]
}

/**
 * Uses stdin as the source for commands; mixin this trait for user interaction.
 */
trait StdIO extends IO {
  protected val commands = io.Source.stdin.getLines
}

abstract class UI extends IO {
  private var canvas = Canvas()

  /**
   * Takes the commands from the iterator and processes them all.
   */
  def run {
    print("Welcome; enter command: ")
    for (command <- commands) {
      if (command == "Q") {print("program finished"); return}
      else process(command)
    }
  }

  /**
   * Processes a command from it's string form
   * @param command The command to execute
   */
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

  /**
   * Formats a command into a list of strings removing whitespaces for easy parsing
   * @param rawCommand a command in it's String form
   * @return a possible parsable list e.g. L 1 1 1 1 -> List("L", "1", "1", "1", "1")
   */
  def format(rawCommand: String): List[String] = {
    rawCommand
      .trim
      .replaceAll("\\s+", " ")
      .split(" ")
      .toList
  }
}

/**
 * Uses an array of commands as the source for them. No user interaction is needed for this
 * @param comm the array of commands.
 */
class UIFromArguments(comm: Array[String]) extends UI {
  protected val commands = comm.toIterator
}