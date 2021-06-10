package fortification.data

class AttackProblemSolution(val attackedFacilityIndexes: IndexedSeq[Int],
                            val objectiveValue: Double,
                            val time: Double,
                            val status: String,
                            val gap: Double,
                            val numCuts: Int = 0,
                            val numEvaluation: Int = 0) {
  override def toString(): String = {
    val attackString = attackedFacilityIndexes.mkString("(", ", ", ")")
    s"${attackString}  objective:${objectiveValue}    time:${time} numEvaluated:${numEvaluation} numCuts:${numCuts}  status:${status}"

  }
}

class RIMPSolution(val fortifiedIndexes: IndexedSeq[Int],
                   val attackedFacilityIndexes: IndexedSeq[Int],
                   val objectiveValue: Double,
                   val time: Double,
                   val status: String,
                   val gap: Double,
                   val numCallsToAttackProblem: Int = 0,
                   val numEvaluation: Int = 0) {
  override def toString(): String = {
    val fortified = fortifiedIndexes.mkString("(", ", ", ")")
    s"Fortified:${fortified}  objective:${objectiveValue}    time:${time} numEvaluated:${numEvaluation} numCallsToAttackProblem:${numCallsToAttackProblem}  status:${status}"

  }
}

class DefendProblemSolution(val openFacilityIndexes: IndexedSeq[Int],
                            val fortifiedIndexes: IndexedSeq[Int],
                            val objectiveValue: Double,
                            val time: Double,
                            val status: String,
                            val gap: Double,
                            val numCuts: Int = 0,
                            val numEvaluation: Int = 0) {
  override def toString(): String = {
    val open = openFacilityIndexes.mkString("(", ", ", ")")
    val fortified = fortifiedIndexes.mkString("(", ", ", ")")
    s"open:${open} fortified:${fortified}  objective:${objectiveValue}    time:${time} numEvaluated:${numEvaluation} numCuts:${numCuts}  status:${status}"

  }
}