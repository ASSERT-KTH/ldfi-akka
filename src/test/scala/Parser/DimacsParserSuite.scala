package Parser
import ldfi.akka.BooleanFormulas.BooleanFormula._
import ldfi.akka.Parser.DimacsParser

import scala.io.Source
import org.scalatest.FunSuite

class DimacsParserSuite extends FunSuite {

  testDimacsParser()

  def testDimacsParser(): Unit = {
    val formula = new Formula
    val clause = new Clause
    val l1 = Message("A", "B", "1")
    val l2 = Message("A", "C", "1")

    clause.addLiteralToClause(l1)
    clause.addLiteralToClause(l2)
    formula.addClause(clause)

    val dimacsTest = "src/test/scala/Parser/dimacsTest.txt"
    DimacsParser.run(formula, dimacsTest)

    val result = Source.fromFile(dimacsTest).mkString

    test("testing DimacsParser"){
      assert(result == "p cnf 2 1" + "\n" + "2 1 0")
    }

  }
}
