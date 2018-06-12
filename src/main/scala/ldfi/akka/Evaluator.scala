package ldfi.akka

import scala.io.Source

import ldfi.akka.BooleanFormulas.BooleanFormula._
import ldfi.akka.BooleanFormulas._
import ldfi.akka.Parser.AkkaParser

import java.lang.reflect.InvocationTargetException
import ldfi.akka.Main.Program



object Evaluator {

  //This can be changed
  val input : Source = Source.fromFile("logs.log")
  val formula = new Formula
  var solToFSpec : Map[Set[Literal], FailureSpec] = Map.empty

  def evaluate(prog: Program): Unit = {

    /************************************************
    Obtain a failure-free outcome of the program
      ************************************************/
    val correctness = forwardStep(prog, Set.empty)
    if (!correctness) {
      sys.error("Forwardstep: program: " + prog + ", does not work even with no failure injections.")
    }
    //Format the program and convert it to CNF
    val format = AkkaParser.parse(input, Set.empty)
    //Convert the formattedlogs to CNF formula
    CNFConverter.run(format, formula)


    /************************************************
    Set initial failure spec and start evaluator
      ************************************************/
    //Initial failurespec from failure-free program
    val initFailureSpec = FailureSpec(
      eot = formula.getLatestTime + 1,
      eff = 2,
      maxCrashes = 0,
      nodes = formula.getAllNodes.toSet,
      messages = formula.getAllMessages.toSet,
      crashes = Set.empty,
      cuts = Set.empty)

    //start evaluator with init failure spec and empty hypothesis
    evaluator(initFailureSpec, Set.empty)

    //Print Failure Injections
    prettyPrintFailureSpecs()

    def evaluator(failureSpec: FailureSpec, hypothesis: Set[Literal]): Unit = {

      println("\n\n**********************************************************************\n" +
        "New run with following injection hypothesis: " + hypothesis + "\n" +
        "And the following failureSpec: " + failureSpec + "\n" +
        "**********************************************************************\n\n")
      val correct = forwardStep(prog, hypothesis)

      //if we did not violate the correctness property we keep looking for failures
      if(correct){
        //update failureSpec with new hypothesis
        val hypcuts = hypothesis.collect { case msg: Message => msg }
        val hypcrashes = hypothesis collect { case n: Node => n }
        val updatedFailureSpec =
          failureSpec.copy(cuts = failureSpec.cuts ++ hypcuts, crashes = failureSpec.crashes ++ hypcrashes)

        //perform the backward step to obtain the new CNF formula
        val newHypotheses = backwardStep(updatedFailureSpec)

        //call evaluator recursively for every hypothesis
        newHypotheses.foreach(hypo => evaluator(updatedFailureSpec, hypo))
      }
      //hypothesis is real solution
      else{
        solToFSpec += (hypothesis -> failureSpec)
      }
    }


  }

  def forwardStep(prog: Program, hypothesis: Set[Literal]): Boolean = {
    //Reset old info in Controller
    Controller.reset()
    //set controller injections
    Controller.setInjections(hypothesis)

    //Create new program and run it
    try {
      prog.mainMethod.setAccessible(true)
      prog.mainMethod.invoke(prog.mainClassInstance)
    } catch {
      case e: InvocationTargetException => sys.error("Invocation of main method failed. " + e.getCause.getMessage)
    }

    val correctness : Boolean = try {
      prog.verifyMethod.setAccessible(true)
      prog.verifyMethod.invoke(prog.verifyClassInstance).asInstanceOf[Boolean]
    } catch {
      case e: InvocationTargetException => sys.error("Invocation of verify method failed. " + e.getCause.getMessage)
    }
    correctness

  }

  def backwardStep(failureSpec: FailureSpec): Set[Set[Literal]] = {
    //Format the program and convert it to CNF
    val format = AkkaParser.parse(input, Set.empty)

    //Convert the formattedlogs to CNF formula
    CNFConverter.run(format, formula)
    CNFConverter.prettyPrintFormula(formula)

    //get new hypotheses from SAT-solver
    val hypotheses = SAT4JSolver.solve(formula, failureSpec)
    hypotheses
  }


  def prettyPrintFailureSpecs(): Unit = {
    println("\n\n" +
      "********************************************************\n" +
      "**************** FAILURE SPECIFICATIONS ****************\n" +
      "********************************************************")
    solToFSpec.foreach( elem =>
      print("\nFailure Specification: " + elem._2)
    )
  }

}
