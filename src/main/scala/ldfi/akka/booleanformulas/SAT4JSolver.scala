package ldfi.akka.booleanformulas

import ldfi.akka.booleanformulas._
import ldfi.akka.FailureSpec
import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory
import org.sat4j.tools.ModelIterator
import scala.collection.mutable.ListBuffer

//influenced by
//https://github.com/palvaro/molly/blob/master/src/main/scala/edu/berkeley/cs/boom/molly/derivations/SAT4JSolver.scala
object SAT4JSolver {

  def solve(formula: Formula, failureSpec: FailureSpec): Set[Set[Literal]] = {

    val solver = SolverFactory.newLight()
    solver.setTimeout(60)
    //Sets are invariant in their type, thus Set[Node] not <: Set[Literal]: we convert to List[T+]
    val allNodes = formula.getAllNodes.toList
    val allMessages = formula.getAllMessages

    val crashedNodes = failureSpec.crashes
    val cutMessages = failureSpec.cuts

    //Add clauses from cnf to solver
    for(c <- formula.clauses){
      val messagesLosses = c.literals collect { case m:MessageLit => m }
      val crashes = c.literals collect { case n:Node => n }
      val vecInts = convertLitsToVecInt(formula, messagesLosses ++ crashes)
      solver.addClause(vecInts)
    }

    //Set new nodes to crash
    for(node <- allNodes){
      val (firstTime, lastTime) = formula.getActivityTimeRange(node.node)
      val crashVars = (firstTime to lastTime).filter(x => formula.literalExistsInFormula(Node(node.node, x))).toArray
      val dummy = solver.nextFreeVarId(true)
      solver.addExactly(new VecInt(crashVars ++ Seq(dummy * -1)), 1)
    }

    //If I have 0 maxcrashes, then no nodes can crash, otherwise atleast #nodes - maxcrashes don't crash
    solver.addAtLeast(convertLitsToNegatedVecInt(formula, allNodes), allNodes.size - failureSpec.maxCrashes)

    //nonAllowedMessageCuts, already cut messages and already crashed nodes are assumed
    val nonAllowedMessageCuts = allMessages.filter(_.time >= failureSpec.eff)
    val assumptions = convertLitsToNegatedVecInt(formula, nonAllowedMessageCuts ++ cutMessages ++ crashedNodes)

    val modelIterator = new ModelIterator(solver)
    val models = ListBuffer[Set[Literal]]()

    while(modelIterator.isSatisfiable(assumptions)){
      val currentModel = modelIterator.model().filter(i => i > 0 && i <= formula.getAllLiterals.size).
        map(formula.getLiteral).toSet
      models += currentModel
    }

    val minimalModels = removeSuperSets(models.toList, models.toList)
    //printModels(minimalModels)
    minimalModels

  }

  def convertLitsToVecInt(formula: Formula, literal: List[Literal]): VecInt = {
    val idList = literal.map(lit => formula.getLiteralId(lit))
    new VecInt(idList.toArray)
  }

  def convertLitsToNegatedVecInt(formula: Formula, literal: List[Literal]): VecInt = {
    val idList = literal.map(lit => formula.getLiteralId(lit) * -1)
    new VecInt(idList.toArray)
  }

  def removeSuperSets (models: List[Set[Literal]], entire: List[Set[Literal]]): Set[Set[Literal]] = models match {
    case Nil => Set.empty
    case head :: tail =>
      val isSuperSet = entire.filter(_ != head).exists { m => m.subsetOf(head) }
      if(isSuperSet) removeSuperSets(tail, entire)
      else Set(head) ++ removeSuperSets(tail, entire)
  }

  def printModels(models : Set[Set[Literal]]): Unit =  {
    for (model <- models) {
      print("\nFault injection: ")
      for (lit <- model) {
        lit match {
          case n: Node => print (n + " ")
          case m: MessageLit => print (m + " ")
        }
      }
    }
  }


}

