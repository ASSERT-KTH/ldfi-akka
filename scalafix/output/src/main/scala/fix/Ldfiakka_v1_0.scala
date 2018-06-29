package fix

import akka.actor._
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import akka.event._
import akka.actor.ActorLogging
import akka.testkit.CallingThreadDispatcher
import ldfi.akka.evaluation.Controller

object Node {
  def props: Props = Props(new Node())
}

class Node extends Actor with ActorLogging {
  val name : String = self.path.name

  def receive = LoggingReceive {
    case _ => if (Controller.greenLight(self, self)) self ! "hello" else {}
  }

  def receiveOther : Receive = LoggingReceive {
    case _ => if (Controller.greenLight(self, self)) self ! "hello" else {}
  }

  val receiveCommand : Receive = LoggingReceive {
    case _ => if (Controller.greenLight(self, self)) self ! "hello" else {}
  }

}

class SimpleDeliv {
  val system : ActorSystem = ActorSystem("system")
  val A : ActorRef = system.actorOf(Node.props.withDispatcher(CallingThreadDispatcher.Id), "A")

  A ! "hello"
  Await.ready(system.whenTerminated, Duration(5, TimeUnit.SECONDS))

}
