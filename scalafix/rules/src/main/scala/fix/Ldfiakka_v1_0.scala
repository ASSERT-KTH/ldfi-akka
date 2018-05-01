
package fix

import scalafix._
import scala.meta._

final case class Ldfiakka_v1_0(index: SemanticdbIndex) extends SemanticRule(index, "Ldfiakka_v1_0") {
  override def fix(ctx: RuleCtx): Patch = {
    //TODO: 1. I add greenlight when the system is sending messages. I should check whether I am inside an actor class.
    //TODO: 2. Fix bug with addExtendsWithActorLogging. It messes up the other pathches when concatenated currently.

    //ctx.debugIndex()
    //println(s"Tree.syntax: " + ctx.tree.syntax)
    //println(s"Tree.structure: " + ctx.tree.structure)

    addControllerGreenLight(ctx) + addLoggingReceive(ctx) + addExtendsWithActorLogging(ctx)
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

  def isActorClassWithNoLogging(templ: Template): Boolean = {
    val inits = templ.inits
    var isExtendedWithActor = false
    var isExtendedWithActorLogging = false
    for(init <- inits){
      if(init.tpe.toString == "Actor"){
        isExtendedWithActor = true
      }
      if(init.tpe.toString == "ActorLogging"){
        isExtendedWithActorLogging = true
      }
    }
    (isExtendedWithActor && !isExtendedWithActorLogging)
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
    ctx.tree.collect {
      case appInf @ Term.ApplyInfix(lhs, op @ Term.Name("!"), _, args) =>
        val newIfTree = getIfTerm(lhs, op, args)
        ctx.replaceTree(appInf, newIfTree.toString)
      case _ => Patch.empty
    }.asPatch
  }

  def getIfTerm(lhs: Term, op: Term.Name, args: List[Term]): Term = {
    val listofargs = List[Term](Term.Name("self"), lhs)
    val condp = Term.Apply(Term.Select(Term.Name("Controller"), Term.Name("greenLight")), listofargs)
    val elsep = Term.Block(List[Stat]())
    val thenp = Term.ApplyInfix(lhs, op, Nil, args)

    Term.If(condp, thenp, elsep)
  }


}