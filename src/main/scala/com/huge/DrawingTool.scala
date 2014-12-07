package com.huge

object DrawingTool {
  /**
   * Entry point; if the program is called without arguments,
   * the program enters into user interaction mode,
   * otherwise, it interprets the args array as the commands to execute.
   * @param args an optional set of commands.
   */
  def main(args: Array[String]): Unit = {
    val ui = 
      if (args.isEmpty) new UI() with StdIO
      else new UIFromArguments(args)
    ui.run
  }
}
