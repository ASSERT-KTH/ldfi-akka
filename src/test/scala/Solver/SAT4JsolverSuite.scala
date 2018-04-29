package Solver

import java.io.{BufferedWriter, FileWriter}

import BooleanFormulas.SAT4Jsolver
import org.scalatest.FunSuite

import scala.io.Source

class SAT4JsolverSuite extends FunSuite {


  testSAT4Jsolver()
  testNegatedSolution()
  testAddNegatedSolutionToFile()

  def testSAT4Jsolver(): Unit = {
    val dimacsTest = "src/test/scala/Solver/dimacsTest.txt"
    val solutionTest = "src/test/scala/Solver/solutionTest.txt"
    val formula = "p cnf 2 1" + "\n" + "2 1 0" //test formula, basically simpledeliv

    //write formula to dimacsTest
    val bw = new BufferedWriter(new FileWriter(dimacsTest))
    bw.write(formula)
    bw.close()

    //solve the formula
    SAT4Jsolver.run(dimacsTest, solutionTest)

    //get solution from solutionTest
    val solution = Source.fromFile(solutionTest).mkString

    test("testing SAT4JSolver"){
      assert(solution == "1 -2 0" +  "\n" +  "-1 2 0")
    }

  }

  def testNegatedSolution(): Unit = {
    val input = Array(1, -2, 3, -4)
    test("Testing negatedSolution"){
      assert(SAT4Jsolver.getNegatedSolution(input) == "-1 2 -3 4 ")
    }
  }

  def testAddNegatedSolutionToFile(): Unit = {
    val filename = "src/test/scala/Solver/dimacsTest.txt"
    val fileContent = Array("p cnf 4 4", "2 1 0")
    val negatedSolution = "-2 1 "
    SAT4Jsolver.addNegatedSolutionToFile(negatedSolution, fileContent, filename)
    val result = Source.fromFile("src/test/scala/Solver/dimacsTest.txt").mkString

    test("Testing addNegatedSolutionToFile") {
      assert(result == "p cnf 5 5" + "\n" + "2 1 0" + "\n" + "-2 1 0")
    }
  }

}