package BooleanFormulas

import java.io._

import ldfi.akka.FailureSpec
import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory
import org.sat4j.reader.DimacsReader
import org.sat4j.reader.ParseFormatException
import org.sat4j.specs.{ContradictionException, IProblem, IVecInt, TimeoutException}
import org.sat4j.tools.ModelIterator

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source




object LightSAT4JSolver {
  /*
  Heavily influenced by https://github.com/palvaro/molly
  Main authors:
  Josh Rosen
  Peter Alvaro
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

    solver.addAtLeast(convertNodesToVecInt(nonCrashedNodes), allNodes.size - failureSpec.maxCrashes)
    solver.addClause(convertNodesToVecInt(allMessages))


    val modelIterator = new ModelIterator(solver)
    //while(modelIterator.isSatisfiable())


  }

  def convertNodesToVecInt(literal: List[Literal]): VecInt = {
    val idList = literal.map(lit => lit.getLiteralId(lit))
    new VecInt(idList.toArray)
  }




}

