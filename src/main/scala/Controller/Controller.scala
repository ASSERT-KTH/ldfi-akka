package Controller
import akka.actor._



object Controller {

  var currentSender: String = _
  var currentRecipient: String = _
  var previousSender: String = _
  var previousRecipient: String = _
  var solutions: List[(String, String, String)] = List.empty

  object Clock {
    var time = 0
    def tick(): Unit = time = time + 1
    def reset(): Unit = time = 0
    def getTime: String = time.toString
  }

  def greenLight(sender: ActorRef, recipient: ActorRef): Boolean = {
    manageClock(sender, recipient)
    val msg = (sender.path.name, recipient.path.name, Clock.getTime)
    val isInjected = !solutions.contains(msg)
    //TODO: Check if crashed
    //val crash = isCrashed(sender, clock.getTime)

    isInjected
  }



  def manageClock(sender: ActorRef, recipient: ActorRef): Unit = {
    currentSender = sender.path.name
    currentRecipient = recipient.path.name
    if(previousSender != currentSender && previousRecipient != currentRecipient){
      Clock.tick()
    }
    previousSender = currentSender
    previousRecipient = currentRecipient
  }

  def setSolutions(slutions: List[(String, String, String)]): Unit = {
    solutions = slutions
  }

  //DIRTY HACK TO RESET
  def reset(): Unit = {
    currentSender = null
    currentRecipient = null
    previousSender = null
    previousRecipient = null
    Clock.reset()
  }


  def printState(): Unit = {
    println("\nController: \n" +
      "currentSender: " + currentSender + "\n" +
      "currentRecipient: " + currentRecipient + "\n" +
      "previousSender: " + previousSender + "\n" +
      "previousRecipient: " + previousRecipient + "\n" +
      "clock: " + Clock.getTime + "\n")
  }

}
