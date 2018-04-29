package fix

import scalafix._
import scala.meta._

final case class Ldfiakka_v1_0(index: SemanticdbIndex) extends SemanticRule(index, "Ldfiakka_v1_0") {
  //TODO: Lös vad som är innanför boolean. Tror jag får helt enkelt skriva det som en term som innehåller strängarna
  //val comp = Term.ApplyInfix(Term.Name("crashes"), Term.Name("&&"), List(), List(Term.Name("inject")))
  override def fix(ctx: RuleCtx): Patch = {
    //TODO: 1. I add greenlight when the system is sending messages. I should check whether I am inside an actor class.
    //TODO: 2. Fix bug with addExtendsWithActorLogging. It messes up the other pathches when concatenated currently.

    //ctx.debugIndex()
    //println(s"Tree.syntax: " + ctx.tree.syntax)
    //println(s"Tree.structure: " + ctx.tree.structure)
    
    addControllerGreenLight(ctx) + addLoggingReceive(ctx) // + addExtendsWithActorLogging(ctx)
  }

  def addExtendsWithActorLogging(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case t @ Defn.Class(dc1, dc2, dc3, dc4, template) =>
        val newTemplate = constructTemplate(template)
        val newClass = Defn.Class(dc1, dc2, dc3, dc4, newTemplate)
        ctx.replaceTree(t, newClass.toString)
    }.asPatch

  }

  def constructTemplate(templ: Template): Template = {
    val inits = templ.inits
    var isExtendedWithActor = false
    for(init <- inits){
      if(init.tpe.toString == "Actor"){
        isExtendedWithActor = true
      }
    }

    if(isExtendedWithActor){
      val newInit = Init(Type.Name("ActorLogging"), Name(""), Nil)
      val newInits = inits :+ newInit
      println("oldinits: " + inits)
      println("newinits: " + newInits)
      Template(templ.early, newInits, templ.self, templ.stats)
    }
    else{
      templ
    }

  }


  def addLoggingReceive (ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case t @ Defn.Def(_, name, _, _, _, body) =>
        if(name.value == "receive"){
          val newBody = Term.Apply(Term.Name("LoggingReceive"), List[Term](body))
          ctx.replaceTree(body, newBody.toString)
        }
        else {
          ctx.replaceTree(name, name.toString)
        }

    }.asPatch
  }


  def addControllerGreenLight(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case appInf:Term.ApplyInfix =>
        val lhs = appInf.lhs
        val op = appInf.op
        val targs = appInf.targs
        val args = appInf.args
        appInf.op match {
          case t:Term.Name =>
            if(t.value == "!") {
              val newIfTree = getIfTerm(lhs, op, args)
              ctx.replaceTree(appInf, newIfTree.toString)
              //ctx.removeTokens(appInf.tokens) + ctx.addRight(t, newIfTree.toString)
            }
            else{
              ctx.replaceTree(t, t.toString)
            }
        }
    }.asPatch
  }

  def getIfTerm(lhs: Term, op: Term.Name, args: List[Term]): Term = {
    val listofargs = List[Term](Term.Name("self"), lhs)
    val condp = Term.Apply(Term.Select(Term.Name("Controller"), Term.Name("greenLight")), listofargs)
    val elsep = Term.Block(List[Stat]())
    val thenp = Term.ApplyInfix(lhs, op, Nil, args)

    Term.If(condp, thenp, elsep)
  }

  def exampleIntReplacer(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case t @ Lit.Int(value) =>
        val newValue = value + 1
        ctx.removeTokens(t.tokens) + ctx.addRight(t, newValue.toString)
    }.asPatch

  }

}
