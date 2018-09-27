package ldfi.akka

import ldfi.akka.booleanformulas.{MessageLit, Node}

case class FSpecBox(initialEff: Int,
                    globalMaxCrashes: Int,
                    fSpecMap: Map[Int, FailureSpec]) {

  require(
    fSpecMap.forall {
      case (_, fSpec) =>
        globalMaxCrashes >= fSpec.crashes.size + fSpec.maxCrashes
    },
    "Can not crash more nodes than globalMaxCrash. " + "\nFspecBox: " + this.toString
  )

  require(
    globalMaxCrashes >= getCrashes.size,
    "Can not have total number of crashed nodes more than global maxCrash. " + "\nFspecBox: " + this.toString)

  require(fSpecMap.forall {
    case (_, fSpec) => fSpec.eot > fSpec.eff
  }, "Can not have EFF >= EOT")

  override def toString: String = {
    val prettyFspecMaps = fSpecMap
      .map {
        case (_, fSpec) =>
          fSpec.toString + "\n"
      }
      .reduceLeft(_ ++ _)

    "\nInitial EFF: " +
      initialEff +
      "\nGlobal MaxCrashes: " +
      globalMaxCrashes +
      "\n" + prettyFspecMaps
  }

  def getNoOfNodes: Int =
    fSpecMap.flatMap {
      case (_, fSpec) => fSpec.nodes
    }.size

  def canEffBeIncremented: Boolean =
    this.fSpecMap.exists {
      case (_, fSpec) =>
        fSpec.eff < fSpec.eot - 1
    }

  def canCrashesBeIncremented: Boolean =
    this.fSpecMap.exists {
      case (_, fSpec) =>
        fSpec.maxCrashes + fSpec.crashes.size < this.globalMaxCrashes
    }

  def getMsgCuts: Set[MessageLit] =
    this.fSpecMap.flatMap { case (_, fSpec) => fSpec.cuts }.toSet

  def getCrashes: Set[Node] =
    this.fSpecMap.flatMap { case (_, fSpec) => fSpec.crashes }.toSet

  def getMaxEOT: Int =
    this.fSpecMap.map {
      case (_, fSpec) =>
        fSpec.eot
    }.max

  def getMaxEFF: Int =
    this.fSpecMap.map {
      case (_, fSpec) =>
        fSpec.eff
    }.max

  def maxNumberOfAssumedCrashes: Int = {
    this.fSpecMap.map {
      case (_, fSpec) =>
        fSpec.crashes.size
    }.max
  }

  def getMaxCrashes: Int =
    this.fSpecMap.map {
      case (_, fSpec) =>
        fSpec.maxCrashes
    }.max

  def getMaximalFspec: (Int, Int, Int) = (getMaxEOT, getMaxEFF, getMaxCrashes)

}
