package fix

import akka.actor._
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import akka.event._
import akka.actor.ActorLogging
import akka.testkit.CallingThreadDispatcher
import ldfi.akka.evaluation.Controller

object HelpActor {
  def props(): Props = Props(new HelpActor())
}

class HelpActor extends Actor with ActorLogging {
  def receive = LoggingReceive {
    case _ => if (Controller.greenLight(self.path.name, sender().path.name, "Helping")) sender() ! "Helping"
  }
}

object NodeActor {
  def props(helpActor: ActorRef): Props = Props(new NodeActor(helpActor))
}

class NodeActor(helpActor: ActorRef) extends Actor with ActorLogging {
  val name : String = self.path.name

  def receive = LoggingReceive {
    case "hello" => if (Controller.greenLight(self.path.name, helpActor.path.name, "hello")) helpActor ! "hello"
    case "howdy" => if (Controller.greenLight(self.path.name, helpActor.path.name, "howdy")) helpActor.tell("howdy", self)
  }

  def receiveOther : Receive = LoggingReceive {
    case _ => if (Controller.greenLight(self.path.name, helpActor.path.name, "hello")) helpActor ! "hello"
  }

  val receiveCommand : Receive = LoggingReceive {
    case _ => if (Controller.greenLight(self.path.name, helpActor.path.name, "hello")) helpActor ! "hello"
  }

}

class SimpleDeliv {
  val system : ActorSystem = ActorSystem("system")

  val helpActor: ActorRef = system.actorOf(HelpActor.props.withDispatcher(CallingThreadDispatcher.Id), "HelpActor")
  val nodeActor : ActorRef = system.actorOf(NodeActor.props(helpActor).withDispatcher(CallingThreadDispatcher.Id), "nodeActor")

  if (Controller.greenLight("deadLetters", nodeActor.path.name, "hello")) nodeActor ! "hello"
  if (Controller.greenLight("deadLetters", nodeActor.path.name, "hello")) nodeActor.tell("hello", system.deadLetters)
  Await.ready(system.whenTerminated, Duration(5, TimeUnit.SECONDS))

}
