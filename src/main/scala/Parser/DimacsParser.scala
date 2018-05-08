package Parser

import java.io.{BufferedWriter, File, FileWriter}

import BooleanFormulas.{Formula, Literal}

import scala.collection.mutable.HashMap

object DimacsParser {
  var literalmap = new HashMap[Literal, Int]()
  def run(formula: Formula, dimacsFormulaFile: String): Unit = {

    val file = new File(dimacsFormulaFile)
    val bw = new BufferedWriter(new FileWriter(file))

    val totalclauses = formula.clauses.size
    var literalid = 0
    var body = ""

    for (clause <- formula.clauses) {
      for (literal <- clause.literals) {
        literalid = formula.getLiteralId(literal)
        if(literalid == -1){
          sys.error("DIMACSPARSER: LITERALID == -1.")
        }
        body = body + literalid.toString() + " "
      }
      body = body + "0" + "\n"
    }
    //write header
    bw.write("p cnf " + formula.literalsToId.size.toString() + " " + totalclauses.toString() + "\n")

    //remove last newline
    body = body.substring(0, body.length() - 1)

    bw.write(body)
    bw.close()
  }


}