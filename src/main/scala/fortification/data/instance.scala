package fortification.data

class ProblemInstance(val problem: String)

class LocationProblemInstance(val demandPoints: IndexedSeq[DemandPoint], val candidateLocations: IndexedSeq[CandidateLocation]) extends ProblemInstance("P-Median") {
  val demandsPointIndexes = 0 until demandPoints.size
  val candidateLocationIndexes = 0 until candidateLocations.size

  // compute distance matrix in miles
  val distance = Array.tabulate(demandPoints.length, candidateLocations.length)((i, j) => {
    GeoComputer.distance(demandPoints(i), candidateLocations(j))
  })
}

object FaileRateGenerator {
  def computeFailRate(problem: LocationProblemInstance, alpha: Double, theta: Double): IndexedSeq[Double] = {

    val newOrleans = Coordinate(30.07, -89.93)

    problem.candidateLocationIndexes.map { i => Math.min(1, alpha * Math.exp(-(GeoComputer.distance(problem.candidateLocations(i), newOrleans) / theta))) }
  }
}

class PMedianProblem(val locationContext: LocationProblemInstance, val p: Int)
    extends ProblemInstance("P-Median problem") {

  import locationContext._

}

class RIMPInstance(val locationContext: DefenderProblemInstance,
                   val openLocationIndexes: Set[Int]) extends ProblemInstance("RIMP") {
  import locationContext._
  import locationContext.locationContext._
}

class AttackerProblemInstance(val locationContext: DefenderProblemInstance,
                              val openLocationIndexes: Set[Int],
                              val fortifiedLocationIndexes: Set[Int],
                              val attackUnitsInSubProblem: Int) extends ProblemInstance("Attacker's problem") {

  import locationContext._
  import locationContext.locationContext._

}

class DefenderProblemInstance(val locationContext: LocationProblemInstance,
                              val defenceUnits: Int,
                              val attackUnits: Int,
                              val alpha: Double,
                              val theta: Double,
                              val w: Double) extends ProblemInstance("Defender's problem") {

  import locationContext._

  val randomFailRate = FaileRateGenerator.computeFailRate(locationContext, alpha, theta)
//  val randomFailRate =  locationContext.candidateLocationIndexes.map { i => 0 }

}

class DefenderProblemInstanceP0(override val locationContext: LocationProblemInstance,
                              override val defenceUnits: Int,
                              override val attackUnits: Int,
                              override val w: Double) extends DefenderProblemInstance(locationContext, defenceUnits, attackUnits, 0, 0, w){
   override val randomFailRate =  locationContext.candidateLocationIndexes.map { i => 0.0 }
}

class DefenderProblemInstanceSpecial(override val locationContext: LocationProblemInstance,
                              override val defenceUnits: Int,
                              override val attackUnits: Int,
                              override val w: Double) extends DefenderProblemInstance(locationContext, defenceUnits, attackUnits, 0, 0, w){
   override val randomFailRate =  IndexedSeq(0.25, 0.014822261602022328, 0.12784332910960622, 0.44089450602870445, 0.06716790208820682)
   
//   locationContext.candidateLocationIndexes.map { i => 0.0 }
}



