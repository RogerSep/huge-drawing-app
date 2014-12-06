package com.huge

object DrawingTool {
  def main(args: Array[String]): Unit = {
    val ui = new UI() with StdIO
    ui.run
  }
}
