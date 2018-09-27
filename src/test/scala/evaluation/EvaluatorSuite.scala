package evaluation

import ldfi.akka._
import ldfi.akka.booleanformulas._
import ldfi.akka.evaluation.Evaluator._
import org.scalatest._

class EvaluatorSuite extends FunSuite with Matchers {

  val msg1: MessageLit = MessageLit("A", "B", 1)("")
  val msg2: MessageLit = MessageLit("B", "C", 2)("")
  val msg3: MessageLit = MessageLit("C", "D", 3)("")
  val node1: Node = Node("A", 1)
  val node2: Node = Node("B", 1)
  val node3: Node = Node("B", 2)
  val node4: Node = Node("C", 2)
  val node5: Node = Node("C", 3)
  val node6: Node = Node("D", 3)

  val messages: Set[MessageLit] = Set(msg1, msg2, msg3)
  val nodes: Set[Node] = Set(node1, node2, node3, node4, node5, node6)

  //no msg cuts or crashes assumed for fspec1
  val fSpec1: FailureSpec =
    FailureSpec(4, 1, 1, nodes, messages, Set.empty, Set.empty)

  //msg3 cut & node6 crashed in assumption for fspec2
  val fSpec2: FailureSpec =
    FailureSpec(4, 1, 1, nodes, messages, Set(node6), Set(msg3))

  val fSpec3: FailureSpec =
    FailureSpec(2, 1, 2, Set(node1, node2), Set(msg1), Set.empty, Set.empty)

  //msg1 cut & node2 crashed in assumption for fspec4
  val fSpec4: FailureSpec =
    FailureSpec(2, 1, 0, Set(node1, node2), Set(msg1), nodes, Set(msg1))

  /*

  case class FailureSpec(eot: Int,
                       eff: Int,
                       maxCrashes: Int,
                       nodes: Set[Node],
                       messages: Set[MessageLit],
                       crashes: Set[Node] = Set.empty,
                       cuts: Set[MessageLit] = Set.empty)
   */

  // /Note that the eff = Int and crashes = Int in the following fspecboxes are arbitrary

  /** ************************************************************************************************
    * FORMULA:
    * M(A, B, 1) ∨ M(B, C, 2) ∨ M(C, D, 3) ∨ P(A, 1) ∨ P(B, 1) ∨ P(B, 2) v P(C, 2) ∨ P(C, 3) ∨ P(D, 3)
    * ************************************************************************************************/


  test("Evaluator.FSpecBox.canEffBeIncremented") {

    //fspec1 and fspec2 can be incremented, so should return true
    FSpecBox(1, 6, Map(1 -> fSpec1, 2 -> fSpec2)).canEffBeIncremented shouldEqual true

    //fspec1 can be incremented and fspec3 can not, so should return true
    FSpecBox(1, 6, Map(1 -> fSpec1, 2 -> fSpec3)).canEffBeIncremented shouldEqual true

    //fspec3 can not be incremented, so should return false
    FSpecBox(1, 6, Map(1 -> fSpec3)).canEffBeIncremented shouldEqual false

    //fspec3 and fspec4 can not be incremented, so should return false
    FSpecBox(1, 6, Map(1 -> fSpec3, 2 -> fSpec4)).canEffBeIncremented shouldEqual false

  }

  test("Evaluator.FSpecBox.canCrashesBeIncremented") {

    //fspec1 and fspec2 can be incremented, so should return true
    FSpecBox(1, 6, Map(1 -> fSpec1, 2 -> fSpec2)).canCrashesBeIncremented shouldEqual true

    //fspec1 can be incremented and fspec3 can not, so should return true
    FSpecBox(1, 6, Map(1 -> fSpec1, 2 -> fSpec3)).canCrashesBeIncremented shouldEqual true

    //fspec4 can not be incremented, so should return false
    FSpecBox(1, 6, Map(1 -> fSpec4)).canCrashesBeIncremented shouldEqual false


  }

  test("Evaluator.FSpecBox.getMsgCuts") {
    //msg3 cut for fspec2 and msg1 cut for fspec1
    FSpecBox(1, 6, Map(1 -> fSpec1, 2 -> fSpec2, 3 -> fSpec3, 4 -> fSpec4)).getMsgCuts shouldEqual Set(
      msg1,
      msg3)
  }

  test("Evaluator.FSpecBox.getCrashes") {
    //msg3 cut for fspec2 and msg1 cut for fspec1
    FSpecBox(1, 6, Map(1 -> fSpec1, 2 -> fSpec2, 3 -> fSpec3, 4 -> fSpec4)).getCrashes shouldEqual nodes
  }

  test("Evaluator.incrementFSpecs") {
    val initFspecBox =
      FSpecBox(1, 6, Map(1 -> fSpec1, 2 -> fSpec2, 3 -> fSpec3, 4 -> fSpec4))

    val fSpec1UpdatedEff = fSpec1.copy(eff = 2)
    val fSpec2UpdatedEff = fSpec2.copy(eff = 2)

    incrementFSpecs(initFspecBox, eff = true, crashes = false) shouldEqual
      FSpecBox(1,
        6,
        Map(1 -> fSpec1UpdatedEff,
          2 -> fSpec2UpdatedEff,
          3 -> fSpec3,
          4 -> fSpec4))

    val fSpec1UpdatedMaxCrashes = fSpec1.copy(maxCrashes = 2)
    val fSpec2UpdatedMaxCrashes = fSpec2.copy(maxCrashes = 2)
    val fSpec3UpdatedMaxCrashes = fSpec3.copy(maxCrashes = 3)


    incrementFSpecs(initFspecBox, eff = false, crashes = true) shouldEqual
      FSpecBox(1,
        6,
        Map(1 -> fSpec1UpdatedMaxCrashes,
          2 -> fSpec2UpdatedMaxCrashes,
          3 -> fSpec3UpdatedMaxCrashes,
          4 -> fSpec4))
  }

  test("Evaluator.createFSpecForClause") {
    val formula = new Formula
    val clause = new Clause(formula)
    messages.foreach(msg => clause.addLiteralToClause(msg))
    nodes.foreach(n => clause.addLiteralToClause(n))
    formula.addClause(clause)

    val fSpecBox = FSpecBox(1, 6, Map.empty)
    val hypothesis: Set[Literal] = Set(msg2, msg3, node5, node6)

    createFSpecForClause(clause, fSpecBox, hypothesis) shouldEqual FailureSpec(
      4,
      1,
      4,
      nodes,
      messages,
      Set(node5, node6),
      Set(msg2, msg3))

  }

}
