package ldfi.akka.BooleanFormulas

import java.io._
import ldfi.akka.BooleanFormulas.BooleanFormula._
import ldfi.akka.FailureSpec
import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory
import org.sat4j.tools.ModelIterator

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
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
object Test extends App {
  val formula = new Formula
  val clause = new Clause
  val msgs = Set(Message("A", "B", 1),  Message("A", "C", 1))
  val nodes = Set(Node("A", 1), Node("B", 1), Node("C", 1))
  msgs.foreach(msg => clause.addLiteralToClause(msg))
  nodes.foreach(n => clause.addLiteralToClause(n))
  formula.addClause(clause)

  val fSpec = FailureSpec(2, 2, 1, nodes, msgs, Set(Node("B", 1)), Set.empty)
  LightSAT4JSolver.solve(formula, fSpec)

}




//Acknowledgments: heavily influenced by https://github.com/palvaro/molly
object LightSAT4JSolver {

  def solve(formula: Formula, failureSpec: FailureSpec): List[Set[Literal]] = {

    //Object to create dummy variables for the sat-solver
    object Dummy {
      var dummyIdToNode : Map[Int, Node] = Map.empty
      var dummyNodeToId : Map[Node, Int] = Map.empty
      var cnt = 0

      def getFreshVar(node: Node) : Int = {
        cnt = cnt + 1
        val dummy = formula.getLitIdCnt + cnt
        dummyIdToNode += (dummy -> node)
        dummyNodeToId += (node -> dummy)
        dummy
      }
    }

    import Dummy._

    val solver = SolverFactory.newLight()
    solver.setTimeout(60)
    val allNodes = formula.getAllNodes
    val allMessages = formula.getAllMessages

    val crashedNodes = failureSpec.crashes
    val cutMessages = failureSpec.messages

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
        case None => sys.error("Node doesn't have any activity. ")
      }
      val dummy = Dummy.getFreshVar(node)
      solver.addExactly(new VecInt(crashVars ++ Seq(dummy * -1)), 1)
    }

    //If I have 0 maxcrashes, then no nodes can crash, otherwise atleast #nodes - maxcrashes don't crash
    solver.addAtLeast(convertLitsToNegatedVecInt(allNodes), allNodes.size - failureSpec.maxCrashes)

    //afterEFFmsgs, already cut messages and already crashed nodes are assumed
    val afterEFFmsgs = allMessages.filter(_.time >= failureSpec.eff)
    val assumptions = convertLitsToNegatedVecInt(afterEFFmsgs ++ cutMessages ++ crashedNodes)

    val modelIterator = new ModelIterator(solver)
    val models = ArrayBuffer[Set[Literal]]()

    while(modelIterator.isSatisfiable(assumptions)){
      val currentModel = modelIterator.model().filter(i => i > 0 && !dummyIdToNode.contains(i)).map(formula.getLiteral).toSet
      models += currentModel
    }

    val minimalModels = removeSuperSets(models, models)
    printModels(minimalModels)
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


  def removeSuperSets(models : ArrayBuffer[Set[Literal]], entire : ArrayBuffer[Set[Literal]]): List[Set[Literal]] = models.size match {
    case 0 => List.empty
    case 1 =>
      val model = models.head
      val isSuperSet = entire.filter(_ != model).exists { m => m.subsetOf(model) }
      if(isSuperSet) List(Set.empty)
      else List(model)
    case _ => (removeSuperSets(ArrayBuffer(models.head), entire) ++ removeSuperSets(models.tail, entire)).filter(_.nonEmpty)
  }

  def printModels(models : List[Set[Literal]]): Unit =  {
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

