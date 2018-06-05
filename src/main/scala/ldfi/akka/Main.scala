package ldfi.akka

import java.io.PrintWriter

object Main {

  def main(args: Array[String]): Unit = {
    new PrintWriter("logs.log") {write("");close()}
    Evaluator.evaluate("SimpleDeliv")
  }
}
