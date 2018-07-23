/*
rule = "class:fix.Ldfiakka_v1_0"
*/
package fix

import akka.actor._
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object HelpActor {
  def props(): Props = Props(new HelpActor())
}

class HelpActor extends Actor {
  def receive = {
    case _ => sender() ! "Helping"
  }
}

object NodeActor {
  def props(helpActor: ActorRef): Props = Props(new NodeActor(helpActor))
}

class NodeActor(helpActor: ActorRef) extends Actor {
  val name : String = self.path.name

  def receive = {
    case "hello" => helpActor ! "hello"
    case "howdy" => helpActor.tell("howdy", self)
  }

  def receiveOther : Receive = {
    case _ => helpActor ! "hello"
  }

  val receiveCommand : Receive = {
    case _ => helpActor ! "hello"
  }

}

class SimpleDeliv {
  val system : ActorSystem = ActorSystem("system")

  val helpActor: ActorRef = system.actorOf(Props[HelpActor], "HelpActor")
  val nodeActor : ActorRef = system.actorOf(NodeActor.props(helpActor), "nodeActor")

  nodeActor ! "hello"
  nodeActor.tell("hello", system.deadLetters)
  Await.ready(system.whenTerminated, Duration(5, TimeUnit.SECONDS))

}
