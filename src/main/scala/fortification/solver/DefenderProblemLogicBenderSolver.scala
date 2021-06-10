package fortification.solver

import fortification.data.timer
import fortification.data.SolverInstructor
import fortification.data.DefenderProblemInstance
import ilog.cplex.IloCplex
import ilog.concert.IloIntVar
import fortification.data.DefendProblemSolution
import ilog.concert.IloNumVar
import ilog.cplex.CpxException
import ilog.cplex.IloCplex.LazyConstraintCallback
import scala.util.control.NonFatal
import fortification.data.AttackerProblemInstance
import fortification.data.PMedianProblem
import fortification.data.InstanceReader
import fortification.data.LocationProblemInstance

class DefenderProblemLogicBenderSolver(val instance: DefenderProblemInstance, val instructor: SolverInstructor) extends timer {
  import instance._
  import instance.locationContext._

  var nbCuts = 0

  var upperBound = Double.MaxValue
  var lowerBound = 0.0

  var numbEvaluation = 0

  class LogicBendersCutLazyConstraint(cplex: IloCplex, open: IndexedSeq[IloIntVar], fortify: IndexedSeq[IloIntVar], phi: IloNumVar) extends LazyConstraintCallback {

    // Need to be override by subclass
    def getTransptCosts(openLocs: Set[Int], fortifyLocs: Set[Int]): Double = {

      Console.withOut(cplex.output) {
        val attackInstance = new AttackerProblemInstance(instance, openLocs, fortifyLocs, attackUnits)

        val instructor = SolverInstructor(3600, 0.001)
        
        new AttackerProblemEnumerateSolver(attackInstance, instructor).solve().get.objectiveValue

//        new AttackerProblemCuttingPlaneSolver(attackInstance, instructor).solve().get.objectiveValue
      }
    }

    def main(): Unit = {
      //      Console.withOut(cplex.output) {
      val openValues = open.map { x => getValue(x) }
      val fortifyValues = fortify.map { x => getValue(x) }

      val setupCosts = candidateLocationIndexes.map { j => openValues(j) * candidateLocations(j).fixedCosts }.sum

      val openLocs = candidateLocationIndexes.filter(j => openValues(j) > 0.5).toSet
      val fortifyLocs = candidateLocationIndexes.filter(j => fortifyValues(j) > 0.5).toSet

      val solutionTrspCosts = getTransptCosts(openLocs, fortifyLocs)

      val phiValue = getValue(phi)
      
//      println(s"Phi:${phiValue}   ActualCost:${solutionTrspCosts}")

      val clb = getBestObjValue()
      val cub = setupCosts + solutionTrspCosts

      if (lowerBound < clb){
        lowerBound = clb
//        println(s"LowerBound:${lowerBound}")
      }

      if (upperBound > cub){
        upperBound = cub
//        println(s"UpperBound:${upperBound}")
      }

      if (lowerBound > 0) {
        if (((upperBound - lowerBound) / lowerBound) < instructor.gap) {
          println("No cut is added due to gap limit reached.")
          abort()
          return
        }
      }

      if (Math.abs(phiValue - solutionTrspCosts) < 10E-5) { return }

      var cut = cplex.linearNumExpr(solutionTrspCosts)

      val nonOpenIndexes = candidateLocationIndexes.toSet.diff(openLocs)
      val openButNotFortifiedLocations = openLocs.diff(fortifyLocs)

      for (j <- nonOpenIndexes) {
        val tempOpen = openLocs + j
        val trspCostsAfterOpenFacility = getTransptCosts(tempOpen, fortifyLocs)
        val marginalBenifitByOpenFacility = trspCostsAfterOpenFacility - solutionTrspCosts
        cut.addTerm(marginalBenifitByOpenFacility, open(j))

        val tempFortify = fortifyLocs + j
        val trspCostsAfterOpenFacilityAndFortify = getTransptCosts(tempOpen, tempFortify)
        val marginal = trspCostsAfterOpenFacilityAndFortify - trspCostsAfterOpenFacility

        cut.addTerm(marginal, fortify(j))
      }

      for (j <- openButNotFortifiedLocations) {
        val tempFortify = fortifyLocs + j
        val trspCostsAfterFortify = getTransptCosts(openLocs, tempFortify)
        val marginal = trspCostsAfterFortify - solutionTrspCosts

        cut.addTerm(marginal, fortify(j))
      }

      nbCuts = nbCuts + 1
      add(cplex.ge(cplex.diff(phi, cut), 0))
    }
    //    }
  }

  def solve(): Option[DefendProblemSolution] = {

    beginTime = System.currentTimeMillis()
    timeLimit = instructor.timeLimit

    var ret: Option[DefendProblemSolution] = None

    val cplex = new IloCplex()

    try {
      val open = Array.tabulate(candidateLocations.size)(i => cplex.boolVar())
      val fortify = Array.tabulate(candidateLocations.size)(i => cplex.boolVar())

      val phi = cplex.numVar(0, Double.MaxValue)

      val locationCosts = candidateLocationIndexes.map { j => cplex.prod(candidateLocations(j).fixedCosts, open(j)) }.fold(cplex.numExpr())(cplex.sum)

      val objective = cplex.sum(locationCosts, phi)

      cplex.addMinimize(objective)
      cplex.setOut(null)
      
      for(j <- candidateLocationIndexes) {
        cplex.addLe(fortify(j), open(j))
      }

      cplex.addLe(fortify.fold(cplex.numExpr())(cplex.sum), defenceUnits)

      cplex.use(new LogicBendersCutLazyConstraint(cplex, open, fortify, phi))

      //      cplex.setParam(IloCplex.DoubleParam.EpGap, 0)
      //      cplex.setParam(IloCplex.DoubleParam.TiLim, instructor.timeLimit)

      if (cplex.solve()) {
        val openValues = candidateLocationIndexes.map { j => (j, cplex.getValue(open(j))) }.toMap
        val openIndexes = openValues.filter(p => p._2 > 0.5).keys.toIndexedSeq

        val fortifyValues = candidateLocationIndexes.map { j => (j, cplex.getValue(fortify(j))) }.toMap
        val fortifyIndexes = fortifyValues.filter(p => p._2 > 0.5).keys.toIndexedSeq

        if (lowerBound < cplex.getBestObjValue) {
          println(s"Lower bound updated by the master MIP bestObjValue ${lowerBound} -> ${cplex.getBestObjValue}")
          lowerBound = cplex.getBestObjValue
        }

        var finalGap = (upperBound - lowerBound) / lowerBound
        if (finalGap < 0) finalGap = 0.0
        println(s"Final Upper bound: ${upperBound}, Lower Bound: ${lowerBound}, Gap: ${finalGap}")
        val status = if (timeLimitReached()) "Time Reached" else "Gap Reached"

        ret = Some(new DefendProblemSolution(openFacilityIndexes = openIndexes,
          fortifiedIndexes = fortifyIndexes,
          objectiveValue = upperBound, time = timeUsed(), gap = finalGap, status = status,
          numCuts = nbCuts, numEvaluation = numbEvaluation))
      } else {
        println(cplex.getStatus)
      }

    } catch {
      case e: CpxException => println("Cplex exception caught: " + e);
      case NonFatal(e)     => println("exception caught: " + e.printStackTrace());
    } finally {
      cplex.end()
    }
    ret
  }

}

object DefenderProblemLogicBenderSolver {
  def main(args: Array[String]): Unit = {
    val outCapture = System.out

    Console.withOut(outCapture) {
      val (demands, dcs) = InstanceReader.readInstanceFrom("input//UCFLData50.txt")

      val alpha = 2.0
      val theta = 400
      val w = 0.6

      val defenceUnits = 2
      val attackUnits = 3

      val locationInstance = new LocationProblemInstance(demands, dcs.take(10))

      val defenderInstance = new DefenderProblemInstance(locationInstance, defenceUnits, attackUnits, alpha, theta, w)

      val instructor = SolverInstructor()

      val solver = new DefenderProblemLogicBenderSolver(defenderInstance, instructor)

      solver.solve() match {
        case Some(sol) => println(sol)
        case _         => println("Error")
      }
    }

  }
}