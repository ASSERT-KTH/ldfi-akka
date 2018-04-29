import java.io.PrintWriter

import BooleanFormulas._
import Parser.{AkkaParser, DimacsParser}
import Controller.Controller
import InteractiveProtocols.RetryDeliv.RetryDeliv
import InteractiveProtocols.SimpleDeliv.SimpleDeliv

import scala.collection.mutable.ListBuffer
import scala.io.Source




//TODO: 1. Extend so that crashes are allowed as well
//TODO: 2. Allow for more than one injection at a time. I.e, cut at time 1 AND at time 4
//TODO: 3. Extend to make sure it takes a Failure Specification into account
//TODO: 4. Make sure that I don't add clauses that already exists in the formula.
object Evaluator {
  //This can be changed
  val input = Source.fromFile("logs.log")
  val dimacsFormulaFile = "dimacs.txt"
  val solutionFile = "solution.txt"

  def evaluateProg(prog: String): Unit = {
    val formula = new Formula
    var realSol = new ListBuffer[(String, String, String)]()
    forwardStep(("", "", ""))

    println("\n\nFault Injections: ")
    realSol.toList.foreach(println)

    def forwardStep(injections: (String, String, String)): Unit = {
      Thread.sleep(2000)
      println("\n\n**********************************************************************\n" +
        "New run with following injection hypothesis: " + injections + "\n" +
        "**********************************************************************\n\n")

      //Reset the internal state of the Controller object. THIS IS A HACK.
      Controller.reset()

      //Clear the logs
      new PrintWriter("logs.log") {write("");close()}

      Controller.setSolutions(List(injections))

      //Create new program and run it
      val program = prog match {
        case "SimpleDeliv" =>
          val simpledeliv = new SimpleDeliv
          simpledeliv.run()
          simpledeliv
        case "RetryDeliv" =>
          val retrydeliv = new RetryDeliv
          retrydeliv.run()
          retrydeliv
      }

      val correct = program match {
        case program:SimpleDeliv =>
          program.verifyPostWithoutAssert()
        case program:RetryDeliv =>
          program.verifyPostWithoutAssert()

      }

      if (!correct) {
        println("POST-CONDITION VIOLATED!")
        realSol += injections
      }
      else {
        val hypothesis = backwardStep()
        hypothesis.foreach(x =>
          if (!realSol.contains(x)) {
            forwardStep(x)
          }
        )
      }
    }


    def backwardStep(): List[(String, String, String)] = {
      //Parse program
      val format = AkkaParser.run(input)
      CNFConverter.run(format, formula)
      CNFConverter.prettyPrintFormula(formula)

      //Encode program in boolean formula
      DimacsParser.run(formula, dimacsFormulaFile)

      //Solve the formula, i.e, get the possible failure injections (hypothesis)
      SAT4Jsolver.run(dimacsFormulaFile, solutionFile)
      SAT4Jsolver.prettyPrintSolution(formula)
      SAT4Jsolver.getPrettySol(formula)

    }

  }


}
