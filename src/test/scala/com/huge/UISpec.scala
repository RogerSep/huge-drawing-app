package com.huge

import org.scalatest._

trait InterceptedIO extends IO {
  private val messages: StringBuffer = new StringBuffer()
  def output: String = messages.toString
  
  override def print(message: String) = messages.append(s"${message}")
  override def println(message: String) = print(s"${message}\n")
}

trait QuitCommand extends InterceptedIO {
  val commands = List("Q").toIterator
}

trait NoQuitCommand extends InterceptedIO {
  val commands = List("A", "B", "C").toIterator
}

class UISpec extends FlatSpec with Matchers {
  "UI" should "quit on command 'Q'" in {
    val ui = new UI with QuitCommand
    ui.run
    ui.output should include ("Welcome; enter command: program finished")
  }

  it should "not quit when there's no 'Q' command" in {
    val ui = new UI with NoQuitCommand
    ui.run
    ui.output should not include ("program finished")
  }

  it should "run completely from a commands array" in {
    val ui = new UIFromArguments(Array(
        "C 20 4",
        "L 1 2 6 2",
        "L 6 3 6 4",
        "R 16 1 20 3",
        "B 10 3 o"
      )) with InterceptedIO

    ui.run
    ui.output should include (
      """----------------------
        _|oooooooooooooooxxxxx|
        _|xxxxxxooooooooox   x|
        _|     xoooooooooxxxxx|
        _|     xoooooooooooooo|
        _----------------------""".stripMargin('_'))
  }
}