
package fix

import scalafix._
import scala.meta._

final case class Ldfiakka_v1_0(index: SemanticdbIndex) extends SemanticRule(index, "Ldfiakka_v1_0") {
  override def fix(ctx: RuleCtx): Patch = {
    //ctx.debugIndex()
    //println(s"Tree.syntax: " + ctx.tree.syntax)
    println(s"Tree.structure: " + ctx.tree.structure)
     addLoggingReceive(ctx) + addExtendsWithActorLogging(ctx) + addControllerGreenLight(ctx)
  }

  def addExtendsWithActorLogging(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case parent @ Defn.Class(_, _, _, _, template) =>
        if(isActorClassWithNoLogging(template)){
          template.inits.lastOption match {
            case Some(parent) => ctx.addRight(parent, " with ActorLogging")
            case None => Patch.empty //This should never happen.
          }
        }
        else Patch.empty
    }.asPatch
  }

  def addLoggingReceive (ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case t @ Defn.Def(_, name, _, _, _, body) if name.value == "receive" =>
        val newBody = Term.Apply(Term.Name("LoggingReceive"), List[Term](body))
        ctx.replaceTree(body, newBody.toString)
      case _ => Patch.empty
    }.asPatch
  }

  def addControllerGreenLight(ctx: RuleCtx): Patch = {
    var patch = Patch.empty
    ctx.tree.collect {
      case templ @ Template (_, inits, _, stats) if isExtendedWithActor(inits) =>
        for(stat <- stats){
          stat.collect  {
            case appInf @ Term.ApplyInfix(lhs, op @ Term.Name("!"), _, args) =>
              val newIfTree = getIfTerm(lhs, op, args)
              patch = patch + ctx.replaceTree(appInf, newIfTree.toString)
            case _ => Patch.empty
          }
        }
    }
    patch
  }

  //Helper functions

  def getIfTerm(lhs: Term, op: Term.Name, args: List[Term]): Term = {
    val listofargs = List[Term](Term.Name("self"), lhs)
    val condp = Term.Apply(Term.Select(Term.Name("Controller"), Term.Name("greenLight")), listofargs)
    val elsep = Term.Block(List[Stat]())
    val thenp = Term.ApplyInfix(lhs, op, Nil, args)

    Term.If(condp, thenp, elsep)
  }

  def isActorClassWithNoLogging(templ: Template): Boolean = {
    val inits = templ.inits
    (isExtendedWithActor(inits) && !isExtendedWithActorLogging(inits))
  }

  def isExtendedWithActor(inits: List[Init]): Boolean = {
    var isExtendedWithActor = false
    for (init <- inits){
      if(init.tpe.toString == "Actor")
        isExtendedWithActor = true
    }
    isExtendedWithActor
  }

  def isExtendedWithActorLogging(inits: List[Init]): Boolean = {
    var isExtendedWithActorLogging = false
    for (init <- inits){
      if(init.tpe.toString == "ActorLogging")
        isExtendedWithActorLogging = true
    }
    isExtendedWithActorLogging
  }
}