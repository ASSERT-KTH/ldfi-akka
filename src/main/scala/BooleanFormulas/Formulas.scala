package BooleanFormulas


import scala.collection.mutable._

class Formula {
  var clauses =  List[Clause]()
  var literals = HashMap[Literal, Int]()
  var literalId = 1

  def addClause(clause: Clause): Unit = {
    clauses = clause :: clauses
  }

  def addLiteralToFormula(literal: Literal): Unit = {
    if (literalExistsInFormula(literal)) {
      //DO NOTHING
    }
    else {
      literals.+=((literal, literalId))
      literalId = literalId + 1
    }
  }

  def literalExistsInFormula(literal: Literal): Boolean = {
    literals.contains(literal)
  }

  def getLiteralId(literal: Literal): Int = {
    literals.get(literal) match {
      case Some(id) => id
      case None => -1
    }
  }

  def getLiteral(literalId: Int): Literal = {
    var lit: Literal = null
    for ((l, v) <- literals) {
      if (v == literalId) {
        lit = l
      }
    }
    lit
  }

}


class Clause(formula: Formula) {
  var literals = List[Literal]()

  def addLiteralToClause(literal: Literal): Unit = {
    formula.addLiteralToFormula(literal)
    literals = literal :: literals
  }

  def literalExistsInFormula(literal: Literal): Boolean = {
    formula.literalExistsInFormula(literal)
  }

}

sealed trait Literal

case class Node(node: String, time: String) extends Literal

case class Message(sender: String, recipient: String, time: String) extends Literal