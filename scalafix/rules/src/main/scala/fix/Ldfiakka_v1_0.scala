
package fix

import scalafix._
import scala.meta._

final case class Ldfiakka_v1_0(index: SemanticdbIndex) extends SemanticRule(index, "Ldfiakka_v1_0") {
  override def fix(ctx: RuleCtx): Patch = {
    //debugRules(ctx)
    importController(ctx) + importDispatcher(ctx) +
      addLoggingReceive(ctx) + addExtendsWithActorLogging(ctx) +
      addControllerGreenLight(ctx) + addDispatcherToProps(ctx)
  }

  //import ldfi.akka.Controller.Controller
  def importController(ctx: RuleCtx): Patch = {
    val importee = Importee.Name(Name.Indeterminate("Controller"))
    val importer = Importer(Term.Name("ldfi.akka.Controller"), List(importee))
    ctx.addGlobalImport(importer)
  }

  //import akka.testkit.CallingThreadDispatcher
  def importDispatcher(ctx: RuleCtx): Patch = {
    val importee = Importee.Name(Name.Indeterminate("CallingThreadDispatcher"))
    val importer = Importer(Term.Name("akka.testkit"), List(importee))
    ctx.addGlobalImport(importer)
  }

  //class _ extends Actor => class _ extends Actor with ActorLogging
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

  //class _ extends Actor => class _ extends Actor with ActorLogging
  def addLoggingReceive (ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case t @ Defn.Def(_, name, _, _, _, body) if name.value == "receive" =>
        ctx.addLeft(body, "LoggingReceive ")
      case _ => Patch.empty
    }.asPatch
  }

  //_ ! _ => if(Controller.greenLight) _ ! _ else {}
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

  //_.actorOf(_, _) => _.actorOf(_.withDispatcher(CallingThreadDispatcher.Id), _)
  def addDispatcherToProps(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case apply @ Term.Apply(fun, args) =>
        val initsActor = fun.collect { case term @ Term.Name(value) if value == "actorOf" => term }.nonEmpty
        if(initsActor){
          args.lift(args.length - 2) match {
            case Some(props) => ctx.addRight(props, ".withDispatcher(CallingThreadDispatcher.Id)")
            case _ => Patch.empty
          }
        }
        else Patch.empty
      case _ => Patch.empty
    }.asPatch
  }

  //Helper functions

  def getIfTerm(lhs: Term, op: Term.Name, args: List[Term]): Term = {
    val listofargs = List[Term](Term.Name("self"), lhs)
    val condp = Term.Apply(Term.Select(Term.Name("Controller"), Term.Name("greenLight")), listofargs)
    val elsep = Term.Block(List[Stat]())
    val thenp = Term.ApplyInfix(lhs, op, Nil, args)

    Term.If(condp, thenp, elsep)
  }

  def isActorClassWithNoLogging(templ: Template): Boolean =
    (isExtendedWithActor(templ.inits) && !isExtendedWithActorLogging(templ.inits))

  def isExtendedWithActor(inits: List[Init]): Boolean = inits.exists(i => i.tpe.toString() == "Actor")

  def isExtendedWithActorLogging(inits: List[Init]): Boolean = inits.exists(i => i.tpe.toString() == "ActorLogging")

  def debugRules(ctx: RuleCtx): Unit = {
    ctx.debugIndex()
    println(s"Tree.syntax: " + ctx.tree.syntax)
    println(s"Tree.structure: " + ctx.tree.structure)
  }

}