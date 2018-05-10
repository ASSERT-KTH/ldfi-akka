package ldfi.akka.BooleanFormulas

import java.io._
import ldfi.akka.BooleanFormulas.BooleanFormula._
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
  val msgs = Set(Message("A", "B", 1),  Message("A", "C", 1))
  val nodes = Set(Node("A", 1), Node("B", 1), Node("C", 1))
  msgs.foreach(msg => clause.addLiteralToClause(msg))
  nodes.foreach(n => clause.addLiteralToClause(n))
  formula.addClause(clause)

  val fSpec = FailureSpec(2, 2, 0, nodes, msgs, Set.empty, Set.empty)
  LightSAT4JSolver.solve(formula, fSpec)

}




//Acknowledgments: heavily influenced by https://github.com/palvaro/molly
object LightSAT4JSolver {



  def solve(formula: Formula, failureSpec: FailureSpec): Unit = {

    object FreshVar {
      var cnt = 0

      def getFreshVar : Int = {
        cnt = cnt + 1
        formula.getLitIdCnt + cnt
      }

    }


    val solver = SolverFactory.newLight()
    solver.setTimeout(60)
    val allNodes = formula.getAllNodes
    val allMessages = formula.getAllMessages

    val crashedNodes = failureSpec.crashes
    val nonCrashedNodes = allNodes.filter(n => !crashedNodes.contains(n))

    val cutMessages = failureSpec.messages
    val nonCrashedMessages = allMessages.filter(msg => !cutMessages.contains(msg))


    //Add clauses from cnf to solver
    for(c <- formula.clauses){
      val messagesLosses = c.literals collect { case m:Message => m }
      val crashes = c.literals collect { case n:Node => n }
      val vecInts = convertLitsToVecInt(messagesLosses ++ crashes)

      solver.addClause(vecInts)
    }

    //Set all the crashed nodes as sole clauses with must satisfy constraint
    for (node <- crashedNodes){
      val nodeId = node.getLiteralId(node)
      //This node is already crashed, so it is set to true in the formula
      solver.addExactly(new VecInt(nodeId), 1)
    }

    //Set new nodes to crash
    for(node <- allNodes){
      val activityRange = formula.getActivityTimeRange(node.node)
      val crashVars = activityRange match {
        case Some((firstTime, lastTime)) =>
          (firstTime to lastTime).filter(x => formula.literalExistsInFormula(Node(node.node, x))).toArray
        case None => sys.error("Node doesn't have any activity. ")
      }
      val dummy = FreshVar.getFreshVar

      solver.addExactly(new VecInt(crashVars ++ Seq(dummy)), 1)
    }


    //If I have 0 maxcrashes, then no nodes can crash, otherwise atleast #nodes - maxcrashes don't crash
    solver.addAtLeast(convertLitsToNegatedVecInt(allNodes), allNodes.size - failureSpec.maxCrashes)

    val afterEFFmsgs = allMessages.filter(_.time >= failureSpec.eff)

    val modelIterator = new ModelIterator(solver)
    //get the hypothesis
    val models = ArrayBuffer[Array[Int]]()

    while(modelIterator.isSatisfiable(convertLitsToVecInt(afterEFFmsgs))){

      //val currentModel = modelIterator.model().filter(_ > 0).map(s => formula.getLiteral(s))
      val currentModel = modelIterator.model().filter(_ > 0).map {
        s =>
        val lit = formula.getLiteral(s)
        if(afterEFFmsgs.contains(lit)) -1 * s
        else s
      }
      models += currentModel
      /*
      if(!currentModel.filter(m => !nonCrashedNodes.contains(m)).isEmpty){
         models += currentModel
      }
      */
    }
    for (model <- models){
      print("\nFault injection: ")
      for (lit <- model){
        formula.getLiteral(lit) match {
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

  def convertLitsToNegatedVecInt(literal: List[Literal]): VecInt = {
    val idList = literal.map(lit => lit.getLiteralId(lit) * -1)
    new VecInt(idList.toArray)
  }





}

