package fix

import scala.meta.Pat.Wildcard
import scalafix._
import scala.meta._

final case class Ldfiakka_v1_0(index: SemanticdbIndex) extends SemanticRule(index, "Ldfiakka_v1_0") {
  override def fix(ctx: RuleCtx): Patch = {
    //debugRules(ctx)
    importAkkaEvent(ctx) + importDispatcher(ctx) + importController(ctx) + importActorLogging(ctx) +
      addLoggingReceive(ctx) + addExtendsWithActorLogging(ctx) +
      addControllerGreenLight(ctx) + addDispatcherToProps(ctx) + addLogging(ctx)
  }

  def addImportsForActorClass(importee: Importee, importer: Importer, ctx: RuleCtx): Patch = {

    //Currently defaulting to not adding defaults
    //TODO: Find better way of checking whether class is an actor class

    //only import to files containing actor classes
    val actorClasses = ctx.tree.collect {
      case parent @ Defn.Class(_, _, _, _, template) if isExtendedWithActor(template.inits) =>
        parent
    }.nonEmpty

    if(actorClasses)
      ctx.addGlobalImport(importer)
    else
      Patch.empty
      //ctx.addGlobalImport(importer)
  }

  //import akka.actor.ActorLogging
  def importActorLogging(ctx: RuleCtx): Patch = {
    val importee = Importee.Name(Name.Indeterminate("ActorLogging"))
    val importer = Importer(Term.Name("akka.actor"), List(importee))

    addImportsForActorClass(importee, importer, ctx)
  }

  //import akka.event._
  def importAkkaEvent(ctx: RuleCtx): Patch = {
    val importee = Importee.Name(Name.Indeterminate("event._"))
    val importer = Importer(Term.Name("akka"), List(importee))

    addImportsForActorClass(importee, importer, ctx)
  }

  //import ldfi.akka.Controller
  def importController(ctx: RuleCtx): Patch = {
    val importee = Importee.Name(Name.Indeterminate("Controller"))
    val importer = Importer(Term.Name("ldfi.akka.evaluation"), List(importee))

    addImportsForActorClass(importee, importer, ctx)
  }

  //import akka.testkit.CallingThreadDispatcher
  def importDispatcher(ctx: RuleCtx): Patch = {
    val importee = Importee.Name(Name.Indeterminate("CallingThreadDispatcher"))
    val importer = Importer(Term.Name("akka.testkit"), List(importee))

    addImportsForActorClass(importee, importer, ctx)
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

  //def receive = {...} => def receive = LoggingReceive = {...}
  def addLoggingReceive (ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case fn @ Defn.Def(_, name, _, _, tpe, body)
        if (name.value == "receive" || tpe.toString == "Some(Receive)") && !hasLoggingReceive(body) =>
        ctx.addLeft(body, "LoggingReceive ")
      case valu @ Defn.Val(_, _ , tpe, body)
        if tpe.toString == "Some(Receive)" && !hasLoggingReceive(body) => ctx.addLeft(body, "LoggingReceive ")
      case _ => Patch.empty
    }.asPatch
  }

  //_ ! _ => if(Controller.greenLight) _ ! _ else {}
  def addControllerGreenLight(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case templ @ Template (_, inits, _, stats) if isExtendedWithActor(inits) && !hasGreenLight(stats) =>
        getControllerPatch(ctx, "self", stats)
      case templ @ Template (_, inits, _, stats) if !hasGreenLight(stats) =>
        getControllerPatch(ctx, "\"deadLetters\"", stats)
    }.asPatch
  }

  //_.actorOf(_, _) => _.actorOf(_.withDispatcher(CallingThreadDispatcher.Id), _)
  def addDispatcherToProps(ctx: RuleCtx): Patch = {
    //already is running on single thread with this specific dispatcher
    if(runsOnCallingThreadDispatcher(ctx)){
      Patch.empty
    }
    else {
      ctx.tree.collect {
        case apply@Term.Apply(fun, args) =>
          val initsActor = fun.collect { case term@Term.Name(value) if value == "actorOf" => term }.nonEmpty
          if (initsActor) {
            args.lift(args.length - 2) match {
              case Some(props) => ctx.addRight(props, ".withDispatcher(CallingThreadDispatcher.Id)")
              case _ => Patch.empty
            }
          }
          else Patch.empty
        case _ => Patch.empty
      }.asPatch
    }
  }

  //when(_)  { case _ => ... } => when(_)  { case _ => log.debug("...") ... } =>
  def addLogging(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case templ @ Template(_, inits, _, stats) if isExtendedWithFSM(inits) =>
        constructLoggingMessage(ctx, stats)
    }.asPatch
  }

  def constructLoggingMessage(ctx: RuleCtx, stats: List[Stat]): Patch = {
    stats.collect {
      case Term.Apply(fun, pfn) if isWhen(fun) || isWhenUnhandled(fun) =>
        val listOfCases = pfn.collect { case pfn: Term.PartialFunction => pfn }
        val head = listOfCases.head

        if(head.cases.nonEmpty) {
          val patches = head.cases.map { cse =>
            cse match {
              case cs@Case(Pat.Bind(Pat.Var(Term.Name(valu)), rhs), _, body) =>
                //println("\nMatching option with var name and ambig body for:\nCase: " + cs)
                val loggingMessage = "log.debug(\" received handled message \" + " + valu + " + \" from \" + sender())" + "\n"
                ctx.addLeft(body, loggingMessage)

              case cs@Case(pat: `Wildcard`, _, body) =>
                val loggingMessage = "log.debug(\" received handled message \" + ev + \" from \" + sender())" + "\n"
                //println("\nMatching option no var name and ambig body with wildcard for : \nCase: " + cs)
                ctx.replaceTree(pat, "ev") + ctx.addLeft(body, loggingMessage)

              case cs@Case(pat, _, body) =>
                //println("\nMatching option no var name and ambig body for: \nCase: " + cs)
                val loggingMessage = "log.debug(\" received handled message \" + ev + \" from \" + sender())" + "\n"
                ctx.addLeft(pat, "ev @ ") + ctx.addLeft(body, loggingMessage)
            }
          }
          if(patches.isEmpty) Patch.empty
          else patches.reduceLeft(_ + _)
        }
        else Patch.empty

      case _ => Patch.empty
    }.reduceLeft(_ + _)
  }

  //Helper functions

  def getControllerPatch(ctx: RuleCtx, ref: String, stats: List[Stat]): Patch = stats match {
    case Nil => Patch.empty
    case head :: tail =>
      val patchList = head.collect {
        case appInf @ Term.ApplyInfix(lhs, op @ Term.Name("!"), _, args) =>
          val newIfTree = constructIfTerm(lhs, args, ref, appInf)
          ctx.replaceTree(appInf, newIfTree.toString())
        case app @ Term.Apply(Term.Select(qual, name), args) if name.value == "tell" =>
          val newIfTree = constructIfTerm(qual, List(args.head), ref, app)
          ctx.replaceTree(app, newIfTree.toString())
        case _ => Patch.empty
      }
      patchList.reduceLeft(_ + _) + getControllerPatch(ctx, ref, tail)
  }

  def constructIfTerm(lhs: Term, args: List[Term], ref: String, thenp: Term): Term = {
    val listOfArgs = if (ref == "self") {
      List[Term](Term.Name(ref + ".path.name"), Term.Name(lhs.toString() + ".path.name")) ::: args
    }
    else {
      List[Term](Term.Name(ref), Term.Name(lhs.toString() + ".path.name")) ::: args
    }
    val condp = Term.Apply(Term.Select(Term.Name("Controller"), Term.Name("greenLight")), listOfArgs)
    val elsep = Lit.Unit()
    //Term.If(condp, Term.Block(List(thenp)), elsep)
    Term.If(condp, thenp, elsep)
  }

  def hasGreenLight(stats: List[Stat]): Boolean = {
    stats.exists { s =>
      s.collect {
        case select @ Term.Select(Term.Name(fst), Term.Name(snd)) if fst == "Controller" && snd == "greenLight" =>
          select
      }.nonEmpty
    }
  }

  def hasLoggingReceive(body: Term): Boolean = {
    body.collect {
      case term @ Term.Name(value) if value == "LoggingReceive" =>
        term
    }.nonEmpty
  }

  def runsOnCallingThreadDispatcher(ctx: RuleCtx): Boolean = {
    ctx.tree.collect {
      case templ @ Template(_, _, _, stats) =>
        stats.exists { s =>

          val withDispatcher = s.collect {
            case name @ Term.Name(valu) if valu == "withDispatcher" =>
              name
          }.nonEmpty

          val callingThreadDispatcherId = s.collect {
            case select @ Term.Select(Term.Name(fst), Term.Name(snd))
              if fst == "CallingThreadDispatcher" && snd == "Id" =>
              select
          }.nonEmpty

          withDispatcher && callingThreadDispatcherId
        }
    }.exists(b => b)
  }

  def isActorClassWithNoLogging(templ: Template): Boolean =
    (isExtendedWithActor(templ.inits) && !isExtendedWithActorLogging(templ.inits))

  def isExtendedWithActor(inits: List[Init]): Boolean = inits.exists(i => i.tpe.toString().contains("Actor"))

  def isExtendedWithActorLogging(inits: List[Init]): Boolean = inits.exists(i => i.tpe.toString() == "ActorLogging")

  def debugRules(ctx: RuleCtx): Unit = {
    ctx.debugIndex()
    println(s"Tree.syntax: " + ctx.tree.syntax)
    println(s"Tree.structure: " + ctx.tree.structure)
  }

  def isExtendedWithFSM(inits: List[Init]): Boolean = inits.exists(i => i.tpe.toString().contains("FSM"))

  def isExtendedWithPersistentFSM(inits: List[Init]): Boolean =
    inits.exists(i => i.tpe.toString().contains("PersistentFSM"))

  def isWhen(fun: Term): Boolean = fun match {
    case Term.Apply(Term.Name("when"), _) => true
    case _ => false
  }

  def isWhenUnhandled(fun: Term): Boolean = fun match {
    case Term.Name("whenUnhandled") => true
    case _ => false
  }

}
