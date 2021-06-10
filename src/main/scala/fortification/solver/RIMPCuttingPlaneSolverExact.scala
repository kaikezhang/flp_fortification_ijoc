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
import fortification.data.RIMPInstance
import fortification.data.RIMPSolution

class RIMPCuttingPlaneSolverExact(val instance: RIMPInstance, val instructor: SolverInstructor) extends timer with TransportationCostsCalculator {
  import instance._
  import instance.locationContext._
  import instance.locationContext.locationContext._

  var nbCuts = 0

  var upperBound = Double.MaxValue
  var lowerBound = 0.0

  var numbEvaluation = 0
  var numAttackProblem = 0

  val openIndexes = openLocationIndexes.toIndexedSeq

  def getTransptCosts(openLocs: Set[Int], fortifyLocs: Set[Int], fixedAttackIndexes: Set[Int] = Set.empty[Int]) = {

    numAttackProblem += 1

    val attackInstance = new AttackerProblemInstance(instance.locationContext, openLocs, fortifyLocs, attackUnits)

    val instructor = SolverInstructor(3600, 0.0001)

    //        val solution  =  new AttackerProblemEnumerateSolver(attackInstance, instructor).solve().get

    val solution = new AttackerProblemCuttingPlaneSolver(attackInstance, instructor, fixedAttackIndexes).solve().get

    numbEvaluation += solution.numEvaluation
    (solution.objectiveValue, solution.attackedFacilityIndexes.toSet)

  }

  class RIMPCutLazyConstraint(cplex: IloCplex, fortify: IndexedSeq[IloIntVar], phi: IloNumVar) extends LazyConstraintCallback {

    def main(): Unit = {

      //      Console.withOut(cplex.output) {
      val fortifyValues = fortify.map { x => getValue(x) }

      val fortifyLocs = (0 until openIndexes.size).filter(j => fortifyValues(j) > 0.5).map { i => openIndexes(i) }.toSet

      //      println(s"fortified: ${fortifyLocs}")

      val (solutionTrspCosts, attacked) = getTransptCosts(openLocationIndexes, fortifyLocs)

      //      println(s"attacks ${attacked}")

      val phiValue = getValue(phi)

      //      println(s"Phi:${phiValue}   ActualCost:${solutionTrspCosts}")

      val clb = getBestObjValue()
      val cub = solutionTrspCosts

      if (lowerBound < clb) {
        lowerBound = clb
        //        println(s"LowerBound:${lowerBound}")
      }

      if (upperBound > cub) {
        upperBound = cub
        //        println(s"UpperBound:${upperBound}")
      }

      if (lowerBound > 0) {
        if (((upperBound - lowerBound) / lowerBound) < instructor.gap) {
          //          println("No cut is added due to gap limit reached.")
          abort()
          return
        }
      }

      if (Math.abs(phiValue - solutionTrspCosts) < 10E-5) { return }

      var cut = cplex.linearNumExpr(solutionTrspCosts)

      //      println(fortifyLocs)

//      val otherLocs = openLocationIndexes.diff(fortifyLocs).diff(attacked)

      //      cut.addTerm(marginal, fortify(openIndexes.indexOf(j)))

//      for (j <- attacked.diff(fortifyLocs)) {
//        val tempFortify = fortifyLocs + j
//        val (trspCostsAfterFortify, currentAttack) = getTransptCosts(openLocationIndexes, tempFortify, attacked)
//        val marginal = trspCostsAfterFortify - solutionTrspCosts
//
//        cut.addTerm(marginal, fortify(openIndexes.indexOf(j)))
//
//      }

      for (j <- openLocationIndexes.diff(fortifyLocs)) {

        val (trspCostsNonFortified, attackedLocs) = getTransptCosts(openLocationIndexes, fortifyLocs, Set(j))

        val tempFortify = fortifyLocs + j

        val trspCostsAfterFortify = getTransptCosts(instance.locationContext,
          openLocationIndexes,
          tempFortify,
          Set(j))
          
        numbEvaluation += 1

        //        println(s"trspCostsNonFortified: ${trspCostsNonFortified}")
        //        println(s"trspCostsAfterFortify: ${trspCostsAfterFortify}")

        val marginal = trspCostsAfterFortify - trspCostsNonFortified
        cut.addTerm(marginal, fortify(openIndexes.indexOf(j)))
      }

      //      println(cut)

      nbCuts = nbCuts + 1
      add(cplex.ge(cplex.diff(phi, cut), 0))
    }
    //    }
  }

  def solve(): Option[RIMPSolution] = {

    beginTime = System.currentTimeMillis()
    timeLimit = instructor.timeLimit

    var ret: Option[RIMPSolution] = None

    val cplex = new IloCplex()

    try {

      //      println(openIndexes)
      val fortify = openIndexes.map { j => cplex.boolVar() }

      val phi = cplex.numVar(0, Double.MaxValue)

      cplex.addMinimize(phi)
      cplex.setOut(null)

      cplex.addLe(fortify.fold(cplex.numExpr())(cplex.sum), defenceUnits)

      cplex.use(new RIMPCutLazyConstraint(cplex, fortify, phi))

      cplex.setParam(IloCplex.DoubleParam.EpGap, 0)
      cplex.setParam(IloCplex.DoubleParam.TiLim, instructor.timeLimit)

      if (cplex.solve()) {

        //        println(cplex.getStatus)

        val fortifyValues = (0 until openIndexes.size).map { j => (openIndexes(j), cplex.getValue(fortify(j))) }.toMap
        val fortifyIndexes = fortifyValues.filter(p => p._2 > 0.5).keys.toIndexedSeq

        if (lowerBound < cplex.getBestObjValue) {
          println(s"Lower bound updated by the master MIP bestObjValue ${lowerBound} -> ${cplex.getBestObjValue}")
          lowerBound = cplex.getBestObjValue
        }

        var finalGap = (upperBound - lowerBound) / lowerBound
        if (finalGap < 0) finalGap = 0.0
        println(s"Final Upper bound: ${upperBound}, Lower Bound: ${lowerBound}, Gap: ${finalGap}")
        val status = if (timeLimitReached()) "Time Reached" else "Gap Reached"

        val attackInstance = new AttackerProblemInstance(instance.locationContext, openLocationIndexes, fortifyIndexes.toSet, attackUnits)
        val solution = new AttackerProblemCuttingPlaneSolver(attackInstance, instructor).solve().get

        numbEvaluation += solution.numEvaluation
        numAttackProblem += 1

        ret = Some(new RIMPSolution(
          fortifiedIndexes = fortifyIndexes,
          attackedFacilityIndexes = solution.attackedFacilityIndexes,
          objectiveValue = solution.objectiveValue, time = timeUsed(), gap = finalGap, status = status,
          numCallsToAttackProblem = numAttackProblem, numEvaluation = numbEvaluation))
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

object RIMPCuttingPlaneSolverExact {
  def main(args: Array[String]): Unit = {
    val outCapture = System.out

    Console.withOut(outCapture) {
      val (demands, dcs) = InstanceReader.readInstanceFrom("input//UCFLData50.txt")

      val alpha = 0.2
      val theta = 400
      val w = 0.4

      val p = 15

      val defenceUnits = 3
      val attackUnits = 3

      val locationInstance = new LocationProblemInstance(demands, dcs)

      val pMedianInstance = new PMedianProblem(locationInstance, p)

      val openLocations = new PMedianSolver(pMedianInstance).solve()

      val defenderInstance = new DefenderProblemInstance(locationInstance, defenceUnits, attackUnits, alpha, theta, w)

      val rimpInstance = new RIMPInstance(defenderInstance, openLocations)

      val instructor = SolverInstructor(gap = 0.0001)

      val solver = new RIMPCuttingPlaneSolverExact(rimpInstance, instructor)

      solver.solve() match {
        case Some(sol) => println(sol)
        case _         => println("Error")
      }

      val solverCuttingPlane = new RIMPZhuSolver(rimpInstance, instructor, "Cutting")

      solverCuttingPlane.solve() match {
        case Some(sol) => {
          println(sol)
          //            resultOut.print(s"${sol.objectiveValue}\t${sol.time}\t${sol.numEvaluation}\t${sol.numCallsToAttackProblem}")
        }
        case _ => print("Error")
      }

      //      println(solver.getTransptCosts(openLocations, Set(0, 1, 8)))
      //
      //      println(solver.getTransptCosts(openLocations, Set(0, 1, 37)))

    }

  }
}