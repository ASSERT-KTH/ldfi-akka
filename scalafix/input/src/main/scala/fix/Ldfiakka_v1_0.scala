/*
rule = "class:fix.Ldfiakka_v1_0"
*/
package fix

import akka.actor._
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Node {
  def props: Props = Props(new Node())
}

class Node extends Actor {
  val name : String = self.path.name

  def receive = {
    case _ => self ! "hello"
  }

  def receiveOther : Receive = {
    case _ => self ! "hello"
  }

  val receiveCommand : Receive = {
    case _ => self ! "hello"
  }

}

class SimpleDeliv {
  val system : ActorSystem = ActorSystem("system")
  val A : ActorRef = system.actorOf(Node.props, "A")

  A ! "hello"
  Await.ready(system.whenTerminated, Duration(5, TimeUnit.SECONDS))

}
