package ldfi.akka.BooleanFormulas

import java.io._
import ldfi.akka.BooleanFormulas.BooleanFormula._
import ldfi.akka.FailureSpec
import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory
import org.sat4j.tools.ModelIterator

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

/*
case class FailureSpec(eot: Int,
                       eff: Int,
                       maxCrashes: Int,
                       nodes: Set[Node],
                       messages: Set[Message],
                       crashes: Set[Node] = Set.empty,
                       cuts: Set[Message] = Set.empty)

*/

//influenced by
//https://github.com/palvaro/molly/blob/master/src/main/scala/edu/berkeley/cs/boom/molly/derivations/SAT4JSolver.scala
object SAT4JSolver {

  def solve(formula: Formula, failureSpec: FailureSpec): Set[Set[Literal]] = {

    val solver = SolverFactory.newLight()
    solver.setTimeout(60)
    //Set is invariant in its type, so I have to convert back to list in order to pass it as argument in convertVec..
    val allNodes = formula.getAllNodes.toList
    val allMessages = formula.getAllMessages.toList

    val crashedNodes = failureSpec.crashes
    val cutMessages = failureSpec.cuts

    //Add clauses from cnf to solver
    for(c <- formula.clauses){
      val messagesLosses = c.literals collect { case m:Message => m }
      val crashes = c.literals collect { case n:Node => n }
      val vecInts = convertLitsToVecInt(messagesLosses ++ crashes)
      solver.addClause(vecInts)
    }

    //Set new nodes to crash
    for(node <- allNodes){
      val activityRange = formula.getActivityTimeRange(node.node)
      val crashVars = activityRange match {
        case Some((firstTime, lastTime)) =>
          (firstTime to lastTime).filter(x => formula.literalExistsInFormula(Node(node.node, x))).toArray
        case None => sys.error("Solver: Node doesn't have any activity. " + node)
      }
      val dummy = solver.nextFreeVarId(true)
      solver.addExactly(new VecInt(crashVars ++ Seq(dummy * -1)), 1)
    }

    //If I have 0 maxcrashes, then no nodes can crash, otherwise atleast #nodes - maxcrashes don't crash
    solver.addAtLeast(convertLitsToNegatedVecInt(allNodes), allNodes.size - failureSpec.maxCrashes)

    //nonAllowedMessageCuts, already cut messages and already crashed nodes are assumed
    val nonAllowedMessageCuts = allMessages.filter(_.time >= failureSpec.eff)
    val assumptions = convertLitsToNegatedVecInt(nonAllowedMessageCuts ++ cutMessages ++ crashedNodes)

    val modelIterator = new ModelIterator(solver)
    val models = ListBuffer[Set[Literal]]()

    while(modelIterator.isSatisfiable(assumptions)){
      val currentModel = modelIterator.model().filter(i => i > 0 && i <= formula.getAllLiterals.size).
        map(formula.getLiteral).toSet
      models += currentModel
    }

    val minimalModels = removeSuperSets(models, models)
    //printModels(minimalModels)
    minimalModels

  }

  def convertLitsToVecInt(literal: List[Literal]): VecInt = {
    val idList = literal.map(lit => lit.getLiteralId(lit))
    new VecInt(idList.toArray)
  }

  def convertLitsToNegatedVecInt(literal: List[Literal]): VecInt = {
    val idList = literal.map(lit => lit.getLiteralId(lit) * -1)
    new VecInt(idList.toArray)
  }


  def removeSuperSets(models : ListBuffer[Set[Literal]], entire : ListBuffer[Set[Literal]]): Set[Set[Literal]] =
    models.size match {
    case 0 => Set.empty
    case 1 =>
      val model = models.head
      val isSuperSet = entire.filter(_ != model).exists { m => m.subsetOf(model) }
      if(isSuperSet) Set(Set.empty)
      else Set(model)
    case _ => (removeSuperSets(ListBuffer(models.head), entire) ++ removeSuperSets(models.tail, entire)).
      filter(_.nonEmpty)
  }

  def printModels(models : Set[Set[Literal]]): Unit =  {
    for (model <- models) {
      print("\nFault injection: ")
      for (lit <- model) {
        lit match {
          case n: Node => print (n + " ")
          case m: Message => print (m + " ")
        }
      }
    }
  }


}

