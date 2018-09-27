package ldfi.akka.evaluation

import java.io.PrintWriter
import java.lang.reflect.InvocationTargetException

import ldfi.akka.booleanformulas._
import ldfi.akka.Main.Program
import ldfi.akka.parser.AkkaParser
import ldfi.akka._

import scala.io.Source

object Evaluator {

  def evaluate(program: Program): Unit = {

    /************************************************
    Obtain a failure-free outcome of the program
      ************************************************/
    val initFormula = new Formula
    val correctness = forwardStep(program, Set.empty, initFormula)
    val input = Source.fromFile("ldfi-akka/logs.log")

    if (!correctness) {
      sys.error(
        "Error. Forwardstep: running main program: " + program.mainClass.getName + ", " +
          "without failure injections violates the correctness specification")
    }
    //Format the program and convert it to CNF
    val format = AkkaParser.parse(input, program.freePassMessages)
    //Convert the formattedlogs to CNF formula
    CNFConverter.run(format, initFormula)

    /************************************************
    Set initial failure spec and start evaluator
      ************************************************/
    //Initial failurespec from failure-free program

    val initFailureSpec = FailureSpec(
      eot = initFormula.getLatestTime + 1,
      eff = 3,
      maxCrashes = 0,
      nodes = initFormula.getAllNodes,
      messages = initFormula.getAllMessages.toSet,
      crashes = Set.empty,
      cuts = Set.empty
    )

    val initFSpecBox = FSpecBox(3, 1, Map(0 -> initFailureSpec))

    val formula = new Formula

    //start evaluator with init failure spec and empty hypothesis
    val (solutions, fSpecBox, resFormula) =
      concreteEvaluator(program, formula, initFSpecBox, Set.empty)

    //Print Failure Injections
    presentResults(solutions, fSpecBox, resFormula)

  }

  def concreteEvaluator(program: Program,
                        formula: Formula,
                        fSpecBox: FSpecBox,
                        triedHypotheses: Set[Set[Literal]])
  : (Map[Set[Literal], FSpecBox], FSpecBox, Formula) = {

    /*
    Put a ceiling to global crashes.
    If global crashes is larger than the intitial amount of nodes, then global crashes = number of nodes.
    Otherwise we keep global crashes.
     */
    val ceiledFspecBox = fSpecBox.copy(
      globalMaxCrashes = ceil(fSpecBox.globalMaxCrashes, fSpecBox.getNoOfNodes))

    evaluator(program,
      formula,
      ceiledFspecBox,
      triedHypotheses,
      Set.empty,
      Map.empty) match {
      case (m, allTriedHypos, resultingFSpecBox) if m.isEmpty =>
        if (resultingFSpecBox.canEffBeIncremented) {

          //increment EFF for all fspecs where it is possible
          val updatedFSpecBox =
            incrementFSpecs(resultingFSpecBox, eff = true, crashes = false)
          //recursively call the function with incremented EFF and updated tried hypotheses
          concreteEvaluator(program,
            formula,
            updatedFSpecBox,
            triedHypotheses ++ allTriedHypos)

        } else if (resultingFSpecBox.canCrashesBeIncremented) {
          //increment Crashes for all fspecs where it is possible
          val updatedFSpecBox =
            incrementFSpecs(resultingFSpecBox, eff = false, crashes = true)

          //recursively call the function with incremented Crashes and updated tried hypotheses
          concreteEvaluator(program,
            formula,
            updatedFSpecBox,
            triedHypotheses ++ allTriedHypos)
        }
        //We did not find any solutions (failure injections that violate cor. spec.) => return fspecbox.
        else {
          (Map.empty, fSpecBox, formula)
        }
      case (solutions, _, resultingFspecBox) =>
        (solutions, resultingFspecBox, formula)
    }
  }

  def evaluator(program: Program,
                formula: Formula,
                fSpecBox: FSpecBox,
                triedHypotheses: Set[Set[Literal]],
                hypothesis: Set[Literal],
                solutions: Map[Set[Literal], FSpecBox])
  : (Map[Set[Literal], FSpecBox], Set[Set[Literal]], FSpecBox) = {

    //Run the forwardstep only if there is a hypothesis OR
    //there is no hypothesis and no tried hypotheses (basically the initial run)

    val correctness = if (hypothesis.nonEmpty) {
      println(
        "\n\n**********************************************************************\n" +
          "New run with following injection hypothesis: " + hypothesis + "\n" +
          //"With the following failureSpec: <" + failureSpec.eot + "," + failureSpec.eff + "," + failureSpec.maxCrashes + ">" +
          //"\nAnd messages: " + failureSpec.messages.toList
          //.sortWith(_.time > _.time)
          //.distinct + "\n" +
          "**********************************************************************\n\n")
      val assumedCuts = fSpecBox.getMsgCuts
      val assumedCrashes = fSpecBox.getCrashes
      val updatedHypothesis = hypothesis ++ assumedCuts ++ assumedCrashes
      forwardStep(program, updatedHypothesis, formula)
    } else {
      false
    }

    //if we did not violate the correctness property we keep looking for failures
    if (correctness || hypothesis.isEmpty) {

      //perform the backward step to obtain the new CNF formula
      val (newHypotheses, updatedFSpecBox) =
        backwardStep(formula, fSpecBox, program.freePassMessages, hypothesis)

      val sortedHypotheses =
        newHypotheses.toList
          .sortWith(getLatestLiteralTime(_) > getLatestLiteralTime(_))
          .toSet

      //We keep evaluating only if we have new hypotheses
      if (sortedHypotheses.nonEmpty) {
        //We keep evaluating only if we have new hypotheses
        evaluateHypotheses(program,
          formula,
          updatedFSpecBox,
          triedHypotheses + hypothesis,
          sortedHypotheses.toList,
          solutions)
      }
      //Backward step did not find any new hypotheses
      else {
        (Map.empty, triedHypotheses + hypothesis, updatedFSpecBox)
      }
    }
    //hypothesis is real solution
    else {
      (solutions + (hypothesis -> fSpecBox),
        triedHypotheses + hypothesis,
        fSpecBox)
    }

  }

  def evaluateHypotheses(program: Program,
                         formula: Formula,
                         fSpecBox: FSpecBox,
                         triedHypotheses: Set[Set[Literal]],
                         hypotheses: List[Set[Literal]],
                         solutions: Map[Set[Literal], FSpecBox])
  : (Map[Set[Literal], FSpecBox], Set[Set[Literal]], FSpecBox) =
    hypotheses match {

      case Nil => (solutions, triedHypotheses, fSpecBox)

      case head :: tail if triedHypotheses.contains(head) =>
        //if the hypothesis has been tried already then just keep iterating
        evaluateHypotheses(program,
          formula,
          fSpecBox,
          triedHypotheses,
          tail,
          solutions)

      case head :: tail if !triedHypotheses.contains(head) =>
        //get the solutions and the recursively tried hypotheses
        val (sols, newlyTriedHypotheses, updatedFSpecBox) =
          evaluator(program,
            formula,
            fSpecBox,
            triedHypotheses,
            head,
            solutions)

        //now call iterator with tail, with updated solutions and hypotheses
        evaluateHypotheses(program,
          formula,
          updatedFSpecBox,
          newlyTriedHypotheses ++ triedHypotheses,
          tail,
          sols ++ solutions)

    }

  def forwardStep(program: Program,
                  hypothesis: Set[Literal],
                  formula: Formula): Boolean = {

    //set controller injections and formula
    Controller.setInjections(hypothesis)
    Controller.setFormula(formula)

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
          "Invocation of verification method failed. " + ite.getCause.getMessage)
      case cce: ClassCastException =>
        sys.error(
          "Casting the verification method's return type to boolean failed. " + cce.getCause.getMessage)
    }
    //return the correctness of the run
    correctness
  }

  def backwardStep(formula: Formula,
                   fSpecBox: FSpecBox,
                   freePassMsgs: List[String],
                   hypothesis: Set[Literal]): (Set[Set[Literal]], FSpecBox) = {

    //Parse and format the program
    val input = Source.fromFile("ldfi-akka/logs.log")
    val format = AkkaParser.parse(input, freePassMsgs)

    //Convert the formattedlogs to CNF formula
    val (newClause, existsInFormula) = CNFConverter.run(format, formula)

    if (formula.getAllLiterals.isEmpty)
      sys.error("Backward step: Formula is empty.")

    CNFConverter.prettyPrintFormula(formula, nodes = false)

    val updatedFSpecBox =
    //if the clause already exists in formula it will not be added to it
      if (existsInFormula) {
        fSpecBox
      } else {
        val newFSpec = createFSpecForClause(newClause, fSpecBox, hypothesis)
        val updatedFSpecMap = fSpecBox.fSpecMap + (newClause.getId -> newFSpec)
        fSpecBox.copy(fSpecMap = updatedFSpecMap)
      }

    //get new hypotheses from SAT-solver
    val hypotheses = SAT4JSolver.solve(formula, updatedFSpecBox)
    (hypotheses, updatedFSpecBox)
  }

  def incrementFSpecs(fSpecBox: FSpecBox,
                      eff: Boolean,
                      crashes: Boolean): FSpecBox = {
    if (eff && crashes)
      sys.error("Evaluator: Should only increment one of crashes and eff")
    val updatedFSpecMap = fSpecBox.fSpecMap.map {
      case (id, fSpec) =>
        if (fSpec.eff < fSpec.eot - 1 && eff) {
          val eff = fSpec.eff
          id -> fSpec.copy(eff = eff + 1)
        } else if (fSpec.maxCrashes + fSpec.crashes.size < fSpecBox.globalMaxCrashes && crashes) {
          val maxCrashes = fSpec.maxCrashes
          id -> fSpec.copy(maxCrashes = maxCrashes + 1)
        } else id -> fSpec
    }
    fSpecBox.copy(fSpecMap = updatedFSpecMap)
  }

  def createFSpecForClause(clause: Clause,
                           fSpecBox: FSpecBox,
                           hypothesis: Set[Literal]): FailureSpec = {

    /*
     i) Collect message omissions and crashes and add to fspec assumptions
     ii) If initialEff is larger than latestTime then we default to EFF = EOT - 1
     iii) maxCrashes is set to either globMaxCrashes - assumed crashes or 0
     */

    val hypcuts = hypothesis.collect { case msg: MessageLit => msg }
    val hypcrashes = hypothesis collect { case n: Node => n }
    val latestTime = clause.getLatestTimeInClause

    val newEff =
      if (fSpecBox.initialEff > latestTime) latestTime
      else fSpecBox.initialEff

    val newMaxCrashes =
      if (fSpecBox.globalMaxCrashes > hypcrashes.size)
        fSpecBox.globalMaxCrashes - hypcrashes.size //hypcrashes.size
      else
        0 //fSpecBox.globalMaxCrashes

    FailureSpec(
      eot = latestTime + 1,
      eff = newEff,
      maxCrashes = newMaxCrashes,
      nodes = clause.getNodesInClause,
      messages = clause.getMessagesInClauseDesc.toSet,
      crashes = hypcrashes,
      cuts = hypcuts
    )
  }


  //***************HELPER FUNCTIONS***************
  def ceil(a: Int, ceil: Int): Int =
    if (ceil > a) a
    else ceil

  def getLatestLiteralTime(sol: Set[Literal]): Int =
    sol.collect {
      case m: MessageLit => m.time
      case n: Node       => n.time
    }.max

  def presentResults(solutions: Map[Set[Literal], FSpecBox],
                     TotalFSpecBox: FSpecBox,
                     formula: Formula): Unit = {
    if (solutions.nonEmpty) {
      val prettySolutionList = solutions
        .map {
          case (failureInjection, fSpecBox) =>
            val maxFSpec = fSpecBox.getMaximalFspec
            getPrettySolution(failureInjection, maxFSpec, formula)
        }
      if (prettySolutionList.isEmpty) {
        val prettyNoSolution = getPrettyNoSolution(TotalFSpecBox, formula)
        println("\n\n" + prettyNoSolution)
        writeToResultsLog(prettyNoSolution)
      }
      //Found solutions
      else {
        val prettySolution =
          "Solutions found." +
            "\nFormula: \n" +
            CNFConverter.getPrettyFormula(formula, nodes = true) +
            "\n\n*********FAILURE INJECTIONS*********\n" +
            prettySolutionList.reduceLeft(_ ++ _)

        println("\n\n" + prettySolution)
        writeToResultsLog(prettySolution)
      }
    }
    //No solutions found.
    else {
      val prettyNoSolution = getPrettyNoSolution(TotalFSpecBox, formula)
      println("\n\n" + prettyNoSolution)
      writeToResultsLog(prettyNoSolution)
    }
  }

  def writeToResultsLog(results: String): Unit = {
    new PrintWriter("ldfi-akka/results.log") {
      write(results)
      close()
    }
  }

  def getPrettyNoSolution(fSpecBox: FSpecBox, formula: Formula): String = {
    val (eot, eff, crashes) = fSpecBox.getMaximalFspec
    val prettyFSpecString = "<" +
      eot + "," +
      eff + "," +
      crashes +
      ">"

    "Formula: \n" +
      CNFConverter.getPrettyFormula(formula, nodes = true) +
      "\nNo failure injections could be found." +
      "\nMaximum failure specification: " + prettyFSpecString +
      "\nTotal FSpecBox: " + fSpecBox.toString
  }

  def getPrettySolution(solution: Set[Literal],
                        fSpecInfo: (Int, Int, Int),
                        formula: Formula): String = {
    val (eot, eff, crashes) = fSpecInfo

    "\nFailure injection: " + solution + " with failure specification: <" +
      eot + "," +
      eff + "," +
      crashes +
      "> violated the correctness specification"
  }
}
