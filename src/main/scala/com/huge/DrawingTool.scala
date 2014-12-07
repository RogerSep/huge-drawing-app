package com.huge

object DrawingTool {
  def main(args: Array[String]): Unit = {
    val ui = 
      if (args.isEmpty) new UI() with StdIO
      else new UIFromArguments(args)
    ui.run
  }
}
