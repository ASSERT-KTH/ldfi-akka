package BooleanFormulas

import java.io._
import BooleanFormulas.BooleanFormula._
import ldfi.akka.FailureSpec
import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory
import org.sat4j.reader.DimacsReader
import org.sat4j.reader.ParseFormatException
import org.sat4j.specs.{ContradictionException, IProblem, IVecInt, TimeoutException}
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
  val msgs = Set(Message("A", "B", "1"),  Message("A", "C", "1"))
  val nodes = Set(Node("A", "1"), Node("B", "1"), Node("C", "1"))
  msgs.foreach(msg => clause.addLiteralToClause(msg))
  nodes.foreach(n => clause.addLiteralToClause(n))
  formula.addClause(clause)

  val fSpec = FailureSpec(2, 2, 0, nodes, msgs, Set.empty, Set.empty)
  LightSAT4JSolver.solve(formula, fSpec)

}

object LightSAT4JSolver {
  /*
  Heavily influenced by https://github.com/palvaro/molly
  */


  def solve(formula: Formula, failureSpec: FailureSpec): Unit = {
    val solver = SolverFactory.newLight()
    solver.setTimeout(60)
    val allNodes = formula.getAllNodes
    val allMessages = formula.getAllMessages

    val crashedNodes = failureSpec.crashes
    val nonCrashedNodes = allNodes.filter(n => !crashedNodes.contains(n))

    val cutMessages = failureSpec.messages
    val nonCrashedMessages = allMessages.filter(msg => !cutMessages.contains(msg))

    for(node <- crashedNodes){
      val nodeId = node.getLiteralId(node)
      //This node is already crashed, so it is set to true in the formula
      solver.addExactly(new VecInt(nodeId), 1)
    }

    solver.addAtLeast(convertLitsToVecInt(nonCrashedNodes), allNodes.size - failureSpec.maxCrashes)


    solver.addClause(convertLitsToVecInt(allMessages))


    val modelIterator = new ModelIterator(solver)
    //get the hypothesis
    val models = ArrayBuffer[Array[Literal]]()
    while(modelIterator.isSatisfiable(convertLitsToVecInt(nonCrashedMessages))){
      val currentModel = modelIterator.model().filter(_ > 0).map(s => formula.getLiteral(s))

      if(!currentModel.filter(m => !nonCrashedNodes.contains(m)).isEmpty){
         models += currentModel
      }
    }

    for (model <- models){
      print("\nFault injection: ")
      for (lit <- model){
        lit match {
          case n:Node => print(n + " ")
          case m:Message => print(m + " ")
        }
      }
    }


  }

  def convertLitsToVecInt(literal: List[Literal]): VecInt = {
    val idList = literal.map(lit => lit.getLiteralId(lit))
    new VecInt(idList.toArray)
  }




}

