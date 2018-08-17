package ldfi.akka.evaluation

import java.io.PrintWriter
import java.lang.reflect.InvocationTargetException

import ldfi.akka.booleanformulas._
import ldfi.akka.Main.Program
import ldfi.akka.parser.AkkaParser
import ldfi.akka.FailureSpec

import scala.io.Source

object Evaluator {

  def evaluate(prog: Program, freePassMessages: List[String]): Unit = {

    /************************************************
    Obtain a failure-free outcome of the program
      ************************************************/
    val tempFormula = new Formula
    val correctness = forwardStep(prog, Set.empty)
    val input = Source.fromFile("ldfi-akka/logs.log")

    if (!correctness) {
      sys.error(
        "Error. Forwardstep: running main program: " + prog.mainClass.getName + ", " +
          "without failure injections violates the correctness specification")
    }
    //Format the program and convert it to CNF
    val format = AkkaParser.parse(input, Set.empty, freePassMessages)
    //Convert the formattedlogs to CNF formula
    CNFConverter.run(format, tempFormula)

    /************************************************
    Set initial failure spec and start evaluator
      ************************************************/
    //Initial failurespec from failure-free program
    val initFailureSpec = FailureSpec(
      eot = tempFormula.getLatestTime + 1,
      eff = 2,
      maxCrashes = 0,
      nodes = tempFormula.getAllNodes.toSet,
      messages = tempFormula.getAllMessages.toSet,
      crashes = Set.empty,
      cuts = Set.empty
    )

    val formula = new Formula

    //start evaluator with init failure spec and empty hypothesis
    val solutions =
      concreteEvaluator(prog,
                        freePassMessages,
                        formula,
                        initFailureSpec,
                        initFailureSpec,
                        Set.empty)

    //Print Failure Injections
    prettyPrintFailureSpecs(solutions)

  }

  def concreteEvaluator(
      program: Program,
      freePassMessages: List[String],
      formula: Formula,
      currentFailureSpec: FailureSpec,
      initFailureSpec: FailureSpec,
      hypothesis: Set[Literal]): Map[Set[Literal], FailureSpec] =
    evaluator(program,
              freePassMessages,
              formula,
              currentFailureSpec,
              hypothesis,
              Map.empty) match {
      case m: Map[Set[Literal], FailureSpec] if m.isEmpty =>
        if (currentFailureSpec.eff < currentFailureSpec.eot - 1) {
          val EFF = currentFailureSpec.eff
          concreteEvaluator(program,
                            freePassMessages,
                            new Formula,
                            initFailureSpec.copy(eff = EFF + 1),
                            initFailureSpec,
                            hypothesis)
        } else if (currentFailureSpec.maxCrashes < formula.getLatestTime - 1) {
          val maxCrashes = currentFailureSpec.maxCrashes
          concreteEvaluator(program,
                            freePassMessages,
                            new Formula,
                            initFailureSpec.copy(maxCrashes = maxCrashes + 1),
                            initFailureSpec,
                            hypothesis)
        } else {
          Map.empty
        }
      case solutions => solutions
    }

  def evaluator(program: Program,
                freePassMessages: List[String],
                formula: Formula,
                failureSpec: FailureSpec,
                hypothesis: Set[Literal],
                solutions: Map[Set[Literal], FailureSpec])
    : Map[Set[Literal], FailureSpec] = {
    if (hypothesis.nonEmpty)
      println(
        "\n\n**********************************************************************\n" +
          "New run with following injection hypothesis: " + hypothesis + "\n" +
          "With the following failureSpec: <" + failureSpec.eot + "," + failureSpec.eff + "," + failureSpec.maxCrashes + ">" +
          "\nAnd messages: " + failureSpec.messages.toList
          .sortWith(_.time > _.time)
          .distinct + "\n" +
          "**********************************************************************\n\n")

    val correctness = forwardStep(
      program,
      hypothesis ++ failureSpec.cuts ++ failureSpec.crashes)

    //if we did not violate the correctness property we keep looking for failures
    if (correctness) {

      //perform the backward step to obtain the new CNF formula
      val (newHypotheses, updatedFailureSpec) =
        backwardStep(formula, failureSpec, freePassMessages, hypothesis)
      val sortedHypotheses =
        newHypotheses.toList
          .sortWith(
            getHypothesisWithLatestEvent(_) > getHypothesisWithLatestEvent(_))
          .toSet

      //We keep evaluating only if we have new hypotheses
      if (sortedHypotheses.nonEmpty) {
        println("\nSortedHypotheses: " + sortedHypotheses + "\n")
        //call evaluator recursively for every hypothesis
        val result = sortedHypotheses.map { hypo =>
          evaluator(program,
                    freePassMessages,
                    formula,
                    updatedFailureSpec,
                    hypo,
                    solutions)
        }
        if (result.isEmpty) Map.empty
        else result.reduceLeft(_ ++ _)
      } else Map.empty
    }
    //hypothesis is real solution
    else {
      solutions + (hypothesis -> failureSpec)
    }
  }

  def forwardStep(program: Program, hypothesis: Set[Literal]): Boolean = {

    //Reset old info in Controller
    Controller.reset()
    //set controller injections
    Controller.setInjections(hypothesis)

    //clear the logs for each run
    new PrintWriter("ldfi-akka/logs.log") {
      write("")
      close()
    }

    //Invoke the main method
    try {
      program.mainMethod.setAccessible(true)
      program.mainMethod.invoke(null, Array[String]())
    } catch {
      case e: InvocationTargetException =>
        sys.error("Invocation of main method failed. " + e.getCause.getMessage)
    }

    //Invoke the verify method
    val correctness = try {
      program.verifyMethod.setAccessible(true)

      //We only create new instance if veryify method is not in the main class
      if (program.verifyClass != program.mainClass) {
        val freshInst = program.verifyClass.newInstance()
        program.verifyMethod.invoke(freshInst).asInstanceOf[Boolean]
      } else {
        program.verifyMethod.invoke(program.verifyClass).asInstanceOf[Boolean]
      }

    } catch {
      case ite: InvocationTargetException =>
        sys.error(
          "Invocation of verify method failed. " + ite.getCause.getMessage)
      case cce: ClassCastException =>
        sys.error(
          "Cast ver method ret type to boolean fail. " + cce.getCause.getMessage)
    }
    //return the correctness of the run
    correctness
  }

  def backwardStep(
      formula: Formula,
      failureSpec: FailureSpec,
      freePassMsgs: List[String],
      hypothesis: Set[Literal]): (Set[Set[Literal]], FailureSpec) = {
    //Parse and format the program
    val input = Source.fromFile("ldfi-akka/logs.log")
    val format = AkkaParser.parse(input, hypothesis, freePassMsgs)

    //Convert the formattedlogs to CNF formula
    CNFConverter.run(format, formula)
    CNFConverter.prettyPrintFormula(formula)

    //update failureSpec with new hypothesis
    val hypcuts = hypothesis.collect { case msg: MessageLit => msg }
    val hypcrashes = hypothesis collect { case n: Node => n }

    val updatedFailureSpec =
      failureSpec.copy(
        eot = formula.getLatestTime + 1,
        nodes = formula.getAllNodes,
        messages = formula.getAllMessages.toSet,
        crashes = failureSpec.crashes ++ hypcrashes,
        cuts = failureSpec.cuts ++ hypcuts
      )

    //get new hypotheses from SAT-solver
    val hypotheses = SAT4JSolver.solve(formula, failureSpec)
    (hypotheses, updatedFailureSpec)
  }

  def getHypothesisWithLatestEvent(sol: Set[Literal]): Int = {
    sol.collect {
      case m: MessageLit => m.time
      case n: Node       => n.time
    }.max
  }

  def prettyPrintFailureSpecs(
      solutions: Map[Set[Literal], FailureSpec]): Unit = {
    println(
      "\n\n" +
        "********************************************************\n" +
        "**************** FAILURE SPECIFICATIONS ****************\n" +
        "********************************************************")
    solutions.foreach { elem =>
      val fSpec = elem._2
      print(
        "\nFailure injection: " + elem._1 + " with failure specification: <" + fSpec.eot + "," + fSpec.eff + "," +
          fSpec.maxCrashes + "> violated the correctness specification")
    }

  }

}
