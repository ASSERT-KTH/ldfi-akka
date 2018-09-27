package ldfi.akka.booleanformulas

import java.io.{IOException, PrintWriter}

import ldfi.akka.FSpecBox
import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory
import org.sat4j.specs.ContradictionException
import org.sat4j.specs.TimeoutException
import org.sat4j.tools.ModelIterator

import scala.collection.mutable.ListBuffer

//influenced by
//https://github.com/palvaro/molly/blob/master/src/main/scala/edu/berkeley/cs/boom/molly/derivations/SAT4JSolver.scala
object SAT4JSolver {

  case class NeverCrashed(node: Node)

  def solve(formula: Formula, fSpecBox: FSpecBox): Set[Set[Literal]] = {

    val solver = SolverFactory.newLight()
    solver.setTimeout(3600)
    //Sets are invariant in their type, thus Set[Node] not <: Set[Literal]: we convert to List[T+]

    //get all crashed nodes and cut messages from the failurespecs in the failurespec map
    val crashedNodes = fSpecBox.getCrashes
    val cutMessages = fSpecBox.getMsgCuts
    val noOfNodes = formula.getNumberOfUniqueNodes
    val maxCrashes = fSpecBox.getMaxCrashes
    val allowedCrashes =
      if (maxCrashes > crashedNodes.size) maxCrashes - crashedNodes.size
      else 0


    //Add clauses from cnf to solver
    for (c <- formula.getClauses) {
      //ignore messages where an actor send a message to themself
      val messageLosses =
        c.getMessagesInClauseAsc.filter(m => m.sender != m.recipient)
      val crashes = c.getNodesInClause
      val vecInts =
        convertLitsToVecInt(formula, messageLosses ++ crashes, not = false)
      solver.addClause(vecInts)
    }

    /*
    Here, we iterate through all of the nodes and create a literal for each activity,
    such that a node can crash at different times. We create a neverCrash literal for each unique node,
    because we do not have crash recoveries, which means that each node either crashes once, or never crashes.
     */
    val neverCrashes = formula.getUniqueNodes.map { node =>
      val (firstTime, lastTime) = formula.getActivityTimeRange(node)
      val crashVars = (firstTime to lastTime)
        .filter(x => formula.literalExistsInFormula(Node(node, x)))
        .map(time => formula.getLiteralId(Node(node, time)))
        .toArray

      val neverCrash = solver.nextFreeVarId(true)

      solver.addExactly(new VecInt(crashVars ++ Seq(neverCrash)), 1)
      neverCrash
    }.toArray

    //If I have 0 maxcrashes, then no nodes can crash, otherwise atleast #nodes - maxcrashes don't crash
    solver.addAtLeast(new VecInt(neverCrashes), noOfNodes - allowedCrashes)

    //The logical clock is reversed such that the "latest" messages are cut first. I.e, if we have
    //M(A, B, 1) V M(A, B, 2) v M(A, B, 3) and EFF = 2, M(A, B, 1) and M(A, B, 2) are disallowed.
    val nonAllowedMessageCuts = formula.clauses.flatMap { c =>
      val messages = c.getMessagesInClauseDesc

      val (eot, eff) = fSpecBox.fSpecMap.get(c.getId) match {
        case Some(fSpec) => (fSpec.eot, fSpec.eff)
        case None =>
          println(fSpecBox.toString)
          formula.clauses.foreach(c => println("clause id: " + c.getId))
          sys.error(
            "Solver: Could not find clause with id: " + c.getId + " in fspec map")
      }
      messages.filter(eot - _.time > eff)
    }.distinct

    /*
    We negate all of the assumptions,
    which by unit propagation will lead to the removal of all non-negated assumptions.
    In other words, we'll ask the solver to give us additional solutions apart from the ones already assumed.
     */

    val assumptions = convertLitsToVecInt(
      formula,
      nonAllowedMessageCuts ++ cutMessages ++ crashedNodes,
      not = true)

    val modelIterator = new ModelIterator(solver)
    val models = ListBuffer[Set[Literal]]()

    try {
      while (modelIterator.isSatisfiable(assumptions)) {
        val currentModel = modelIterator
          .model()
          .filter(i => i > 0 && i <= formula.getAllLiterals.size)
          .map(formula.getLiteral)
          .toSet
        models += currentModel
      }
    } catch {
      case ioe: IOException =>
        new PrintWriter("ldfi-akka/solverlog.log") {
          write(getIOEString(formula, fSpecBox))
          close()
        }
        sys.error("Solver: IO exception.")
      case te: TimeoutException =>
        new PrintWriter("ldfi-akka/solverlog.log") {
          write(getTOEString(formula, fSpecBox))
          close()
        }
        sys.error("Solver: Timeout exception.")
      case ce: ContradictionException =>
        new PrintWriter("ldfi-akka/solverlog.log") {
          write(getCEString(formula, fSpecBox))
          close()
        }
        sys.error("Solver: Contradiction exception.")
    }

    val minimalModels = removeSuperSets(models.toList, models.toList)
    //printModels(minimalModels)
    minimalModels

  }

  def convertLitsToVecInt(formula: Formula,
                          literal: List[Literal],
                          not: Boolean): VecInt = {
    if (not) new VecInt(literal.map(l => formula.getLiteralId(l) * -1).toArray)
    else new VecInt(literal.map(formula.getLiteralId).toArray)
  }

  def removeSuperSets(models: List[Set[Literal]],
                      entire: List[Set[Literal]]): Set[Set[Literal]] =
    models match {
      case Nil => Set.empty
      case head :: tail =>
        val isSuperSet = entire.filter(_ != head).exists(m => m.subsetOf(head))
        if (isSuperSet) removeSuperSets(tail, entire)
        else Set(head) ++ removeSuperSets(tail, entire)
    }

  def printModels(models: Set[Set[Literal]]): Unit = {
    for (model <- models) {
      print("\nFault injection: ")
      for (lit <- model) {
        lit match {
          case n: Node       => print(n + " ")
          case m: MessageLit => print(m + " ")
        }
      }
    }
  }

  def getIOEString(formula: Formula, fSpecBox: FSpecBox): String =
    "\n*************IO EXCEPTION*************\n" +
      getFormulaAndFspecString(formula, fSpecBox)

  def getCEString(formula: Formula, fSpecBox: FSpecBox): String =
    "\n********CONTRADICTION EXCEPTION*******\n" +
      getFormulaAndFspecString(formula, fSpecBox)

  def getTOEString(formula: Formula, fSpecBox: FSpecBox): String =
    "\n**********TIMEOUT EXCEPTION**********\n" +
      getFormulaAndFspecString(formula, fSpecBox)

  def getFormulaAndFspecString(formula: Formula, fSpecBox: FSpecBox): String =
    "\nFormula: \n" +
      CNFConverter.getPrettyFormula(formula, nodes = false) +
      "\nFspecbox: " +
      fSpecBox.toString

}
