package ldfi.akka

import ldfi.akka.booleanformulas._

case class FailureSpec(eot: Int,
                       eff: Int,
                       maxCrashes: Int,
                       nodes: Set[Node],
                       messages: Set[MessageLit],
                       crashes: Set[Node] = Set.empty,
                       cuts: Set[MessageLit] = Set.empty) {

  override def toString: String = {
    "Failure Specification: <" +
      eot + "," +
      eff + "," +
      maxCrashes +
      ">"
  }

  def longToString: String = {
    "Failure Specification: <\n" +
      eot + ", \n" +
      eff + ", \n" +
      maxCrashes + ", \n" +
      nodes + ", \n" +
      messages + ", \n" +
      crashes + ", \n" +
      cuts +
      "\n>"

  }

  //Can not be upheld since amount of nodes differ across clauses and fspecs.
  //require(maxCrashes <= nodes.size, "Can't have more crashes than nodes. \n" + longToString)

  //Can be upheld. Since EOT and EFFs are relative.
  require(eot > eff, "EOT cant be larger than EFF. \n" + longToString)

  //Can not be upheld. Again, number of nodes differ across clauses and fspecs.
  //require(crashes.size <= maxCrashes, "Can't have more crashed nodes than maxCrashes. \n" + longToString)

  //Can not be upheld. I might have cuts at a time specific for a different clause > EFF or EOT for this clause.
  //require(cuts.forall(eot - _.time <= eff), "Can't have omissions after the EFF. \n" + longToString)


}




