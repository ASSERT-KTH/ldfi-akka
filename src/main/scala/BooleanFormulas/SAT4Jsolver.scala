package BooleanFormulas

import java.io._

import org.sat4j.minisat.SolverFactory
import org.sat4j.reader.DimacsReader
import org.sat4j.reader.ParseFormatException
import org.sat4j.specs.ContradictionException
import org.sat4j.specs.IProblem
import org.sat4j.specs.TimeoutException

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source


object SAT4Jsolver {

  def run(dimacsFormulaFile: String, solutionFile: String): Unit = {
    val solver = SolverFactory.newDefault()
    solver.setTimeout(60)
    val reader = new DimacsReader(solver)
    val file = new File(solutionFile)
    val bw = new BufferedWriter(new FileWriter(file))
    var negatedSolution, currentSol = ""
    var minimalCnt = Integer.MAX_VALUE
    var minimalSolution = mutable.Set[String]()

    try {
      var problem = reader.parseInstance(dimacsFormulaFile)
      // GET MINIMAL SOLUTIONS
      while (problem.isSatisfiable) {
        currentSol = reader.decode(problem.model)
        //if a new minimal solution is found, delete all old solutions
        if (countMinimal(currentSol) < minimalCnt) {
          minimalCnt = countMinimal(currentSol)
          minimalSolution.clear()
          minimalSolution += currentSol
        }
        else if (countMinimal(currentSol) == minimalCnt) {
          minimalSolution += currentSol
        }
        var fileContent = Source.fromFile(dimacsFormulaFile).mkString.split("\n")
        negatedSolution = getNegatedSolution(problem.model())
        addNegatedSolutionToFile(negatedSolution, fileContent, dimacsFormulaFile)
        problem = reader.parseInstance(dimacsFormulaFile)
      }
      //Get the minimal solutions in pretty format and write to solution.txt
      val prettyMinSol = getPrettyMinSol(minimalSolution)
      bw.write(prettyMinSol)
      bw.close()
    }
    catch {
      case e: FileNotFoundException => e.printStackTrace()
      case e: ParseFormatException => e.printStackTrace()
      case e: IOException => e.printStackTrace()
      case e: ContradictionException => e.printStackTrace()
      case e: TimeoutException => e.printStackTrace()
    }
  }

  def getNegatedSolution(problem: Array[Int]): String = {
    var negatedSolution = ""
    for (i <- problem) {
      val pm = i.toString()
      if (pm.charAt(0) == '-') {
        negatedSolution = negatedSolution + pm.charAt(1).toString + " "
      }
      else {
        negatedSolution = negatedSolution + "-" + pm + " "
      }
    }
    //remove last whitespace
    //negatedSolution.substring(0, negatedSolution.length - 1)
    negatedSolution
  }



  def addNegatedSolutionToFile(negatedSolution: String, fileContent: Array[String], fileToWriteSolution: String): Unit = {
    val bw = new BufferedWriter(new FileWriter(fileToWriteSolution))
    var clauses, reversed, newclauses, updatedline = ""
    var cnt, linelength, it = 0
    for (line <- fileContent) {
      linelength = line.size
      //Updated text file with one added clause in the first line
      if (cnt == 0) {
        it = linelength - 1
        while (line.charAt(it) != ' ') {
          clauses = clauses + line.charAt(it).toString
          it = it - 1
        }
        reversed = clauses.reverse
        newclauses = (reversed.toInt + 1).toString
        updatedline = line.replaceAll(reversed, newclauses)
        bw.write(updatedline + "\n")
      }
      //Do nothing
      else {
        bw.write(line + "\n")
      }
      cnt = cnt + 1
    }
    //Add the negated solution the clause
    bw.write(negatedSolution + "0")
    bw.close()
  }

  def countMinimal(solution: String): Int = {
    val iterator = solution.split(" ")
    var cnt = 0
    for (i <- iterator) {
      if (i.charAt(0) != '-') {
        cnt = cnt + 1
      }
    }
    cnt
  }

  def getPrettyMinSol(minimalSolution: mutable.Set[String]): String = {
    var it = 0
    var prettySol = ""
    for (sol <- minimalSolution) {
      prettySol += sol
      if (it != minimalSolution.size - 1) {
        prettySol += "\n"
      }
      it = it + 1
    }
    prettySol
  }


  def prettyPrintSolution(formula: Formula): Unit = {
    println("\nSolution: ")
    val file = new File("solution.txt")
    var it = 0
    var prettySol = ""
    for (line <- Source.fromFile(file).getLines) {
      prettySol += "{"
      val solArray = line.split(" ")
      for (s <- solArray) {
        if (!s.contains('-') && !s.contains("0")) {
          formula.getLiteral(s.toInt) match {
            case Node(node, time) => prettySol += "P(" + node + ", " + time + "), "
            case Message(sender, recipient, time) => prettySol += "M(" + sender + ", " + recipient + ", " + time + "), "
            case _ => sys.error("ERROR IN PRETTYPRINTSOLUTION. LITERAL IS NOT MESSAGE OR NODE.")
          }
        }
      }
      //Remove last comma and space
      prettySol = prettySol.substring(0, prettySol.length - 2)
      prettySol += "}"
      if(it != (Source.fromFile(file).getLines.length - 1)){
        prettySol += ", \n"
      }
      it = it + 1
    }
    print(prettySol)
  }

  def getPrettySol(formula: Formula): List[(String, String, String)] = {
    var res = List[(String, String, String)]()
    val file = new File("solution.txt")
    var it = 0
    var prettySol = ""
    for (line <- Source.fromFile(file).getLines) {
      prettySol += "{"
      val solArray = line.split(" ")
      for (s <- solArray) {
        if (!s.contains('-') && !s.contains("0")) {
          formula.getLiteral(s.toInt) match {
            case Node(node, time) => prettySol += "P(" + node + ", " + time + "), "; //res + (node, time)
            case Message(sender, recipient, time) => prettySol += "M(" + sender + ", " + recipient + ", " + time + "), "; res = (sender, recipient, time) :: res
            case _ => sys.error("ERROR IN PRETTYPRINTSOLUTION. LITERAL IS NOT MESSAGE OR NODE.")
          }
        }
      }
      //Remove last comma and space
      prettySol = prettySol.substring(0, prettySol.length - 2)
      prettySol += "}"
      if(it != (Source.fromFile(file).getLines.length - 1)){
        prettySol += ", \n"
      }
      it = it + 1
    }
    res
  }

}

