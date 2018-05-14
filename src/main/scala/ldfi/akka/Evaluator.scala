package ldfi.akka

import java.io.PrintWriter

import BooleanFormulas._
import InteractiveProtocols.RetryDeliv.RetryDeliv
import InteractiveProtocols.SimpleDeliv.SimpleDeliv
import ldfi.akka.BooleanFormulas.BooleanFormula.{Formula, Literal, Message, Node}
import ldfi.akka.Parser.{AkkaParser, DimacsParser}

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
    val formula = new BooleanFormula.Formula
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
      Controller.Controller.reset()

      //Clear the logs
      new PrintWriter("logs.log") {write("");close()}

      Controller.Controller.setSolutions(List(injections))

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
      val format = AkkaParser.parse(input, Set.empty)
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

  /*

  case class FailureSpec(eot: Int,
                       eff: Int,
                       maxCrashes: Int,
                       nodes: Set[Node],
                       messages: Set[Message],
                       crashes: Set[Node] = Set.empty,
                       cuts: Set[Message] = Set.empty)
   */

  def evaluate(prog: String): Unit = {
    var initialRun = false
    val formula = new Formula





    def evaluator(): Unit = {

      //Obtain a failure-free outcome of the program
      //val dummyFailureSpec = FailureSpec(0, 0, 0, Set.empty, Set.empty, Set.empty, Set.empty)
      val correctness = forwardStep(Set.empty)
      if(!correctness){
        sys.error("Forwardstep: program: " + prog + ", does not work even with no failure injections.")
      }

      //Initial failurespec from failure-free program
      val initFailureSpec = FailureSpec(
        eot = formula.getLatestTime + 1,
        eff = 2,
        maxCrashes = 0,
        nodes = formula.getAllNodes.toSet,
        messages = formula.getAllMessages.toSet,
        crashes = Set.empty,
        cuts = Set.empty)

      val adjustedFailureSpec = sweep(initFailureSpec)


    }

    def forwardStep(hypothesis: Set[Literal]): Boolean = {
      if(hypothesis.nonEmpty){
        //set controller injections
      }
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

      val correctness = program match {
        case program:SimpleDeliv =>
          program.verifyPostWithoutAssert()
        case program:RetryDeliv =>
          program.verifyPostWithoutAssert()
      }

      correctness
    }
/*
    def hazardAnalysis(failureSpec: FailureSpec, hypothesis: Set[Literal]): Unit = {
       if(forwardStep(hypothesis)){
         val hypcuts = hypothesis.collect { case msg:Message => msg }
         val hypcrashes = hypothesis collect { case n:Node => n}
         val updatedFailureSpec =
          failureSpec.copy(cuts = failureSpec.cuts ++ hypcuts, crashes = failureSpec.crashes ++ hypcrashes)
         val newHypothesis = LightSAT4JSolver.solve(formula, updatedFailureSpec)
         newHypothesis.foreach(hyp => hazardAnalysis(updatedFailureSpec, hyp))

       }
       else {

       }

    }
*/
    def backwardStep(failureSpec: FailureSpec): Unit = {
      //Format the program and convert it to CNF
      val format = AkkaParser.parse(input, Set.empty)
      CNFConverter.run(format, formula)

    }


    def sweep(failureSpec: FailureSpec): FailureSpec = {
      //Get the hypothesis for the fspec
      val hypothesis = LightSAT4JSolver.solve(formula, failureSpec)

      //If the correctness is not violated then we increase eff
      if(!hypothesis.forall(forwardStep)){
        failureSpec
      }
      else {
        //We can not naively fail the program
        if(failureSpec.eot == failureSpec.eff - 1)
          failureSpec
        //increment eff
        else {
          sweep(failureSpec.copy(eff = failureSpec.eff + 1))
        }
      }

    }


















  }


}
