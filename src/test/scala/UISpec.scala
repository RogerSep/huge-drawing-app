import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import com.huge._

trait InterceptedIO extends IO {
  var messages: StringBuffer = new StringBuffer()
  def output: String = messages.toString
  
  override def print(message: String) = messages.append(s"${message}")
  override def println(message: String) = print(s"{message}\n")
}

trait QuitCommand extends InterceptedIO {
  val commands = List("Q").toIterator
}

trait NoQuitCommand extends InterceptedIO {
  val commands = List("A", "B", "C").toIterator
}

class UISpec extends FlatSpec with ShouldMatchers {
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
}