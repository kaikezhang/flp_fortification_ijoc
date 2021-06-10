package fortification.solver

import fortification.data.AttackerProblemInstance
import fortification.data.AttackProblemSolution
import ilog.cplex.IloCplex
import ilog.concert.IloIntVar
import ilog.concert.IloNumVar
import ilog.cplex.CpxException
import ilog.cplex.IloCplex.LazyConstraintCallback
import scala.util.control.NonFatal
import fortification.data.SolverInstructor
import fortification.data.timer
import fortification.data.AttackProblemSolution
import fortification.data.PMedianProblem
import fortification.data.InstanceReader
import fortification.data.LocationProblemInstance
import fortification.data.DefenderProblemInstance

class AttackerProblemCuttingPlaneSolver(val instance: AttackerProblemInstance, 
                                        val instructor: SolverInstructor = new SolverInstructor(3600, 0.005),
                                        val fixedAttack: Set[Int] = Set.empty[Int]) extends timer with TransportationCostsCalculator {
  import instance._
  import instance.locationContext._
  import instance.locationContext.locationContext._

  var nbCuts = 0

  var upperBound = 0.0
  var lowerBound = Double.MaxValue

  var numbEvaluation = 0

  val openLocationIndexesSeq = openLocationIndexes.toIndexedSeq

  var orderedOpenDCs: IndexedSeq[Seq[Int]] = _

  var phi0: Double = _

  class SuperModularCutLazyConstraint(cplex: IloCplex, attack: IndexedSeq[IloIntVar], phi: IloNumVar) extends LazyConstraintCallback {

    def main(): Unit = {

      val attackValues = attack.map { x => getValue(x) }
      
//      println( (0 until openLocationIndexesSeq.size).map(j => attackValues(j) ) )

      val attackFacilityIndexes = (0 until openLocationIndexesSeq.size).filter(j => attackValues(j) > 0.5).map { j => openLocationIndexesSeq(j) }.toSet

      numbEvaluation += 1

      val solutionTrspCosts = getTransptCosts(instance.locationContext, orderedOpenDCs, openLocationIndexes, fortifiedLocationIndexes, attackFacilityIndexes)

      val phiValue = getValue(phi)

      val clb = -getBestObjValue() + phi0
      val cub = solutionTrspCosts

      if (lowerBound > clb)
        lowerBound = clb

      if (upperBound < cub)
        upperBound = cub

//      println("phiValue:" + phiValue)
//      println("solutionTrspCosts:" + solutionTrspCosts)
//      println("attackFacilityIndexes: " + attackFacilityIndexes)
////      println("notSelected: " + notSelected)
//      println("upperBound " + upperBound)
//      println("lowerBound " + lowerBound)
      
      if (upperBound > 0) {
        if (((lowerBound - upperBound) / upperBound) < instructor.gap) {
//          println("No cut is added due to gap limit reached.")
            if(nbCuts > 10)
               abort()
          return
        }
      }
      

      if (Math.abs(phiValue + (solutionTrspCosts - phi0)) < 10E-5) {
//        println("No cut is added due to phi is valid.")
        return
      }

      var cut = cplex.linearNumExpr(0.0)

      val notSelected = openLocationIndexes.diff(attackFacilityIndexes)
      
      

      var tempAttacks = Set.empty[Int]

      var sinkCosts = phi0

      for (j <- attackFacilityIndexes) {
        numbEvaluation += 1
        tempAttacks = tempAttacks + j
        val currentCosts = getTransptCosts(instance.locationContext, orderedOpenDCs, openLocationIndexes, fortifiedLocationIndexes, tempAttacks)
        val incremental = currentCosts - sinkCosts
        sinkCosts = currentCosts
        cut.addTerm(-incremental, attack(openLocationIndexesSeq.indexOf(j)))
      }

      for (j <- notSelected) {
        numbEvaluation += 1
        tempAttacks = tempAttacks + j
        val currentCosts = getTransptCosts(instance.locationContext, orderedOpenDCs, openLocationIndexes, fortifiedLocationIndexes, tempAttacks)
        val incremental = currentCosts - sinkCosts
        sinkCosts = currentCosts
        cut.addTerm(-incremental, attack(openLocationIndexesSeq.indexOf(j)))
      }

      nbCuts = nbCuts + 1
//      println(cut)
      add(cplex.ge(cplex.diff(phi, cut), 0))
    }
  }

  def solve(): Option[AttackProblemSolution] = {
    beginTime = System.currentTimeMillis()
    timeLimit = instructor.timeLimit

    orderedOpenDCs = demandsPointIndexes.map { i =>
      {
        openLocationIndexes.toSeq.sortBy { j => distance(i)(j) }
      }
    }

    phi0 = getTransptCosts(instance.locationContext, orderedOpenDCs, openLocationIndexes, fortifiedLocationIndexes, Set.empty[Int])

//    println("phi0: " + phi0)
    
    if (openLocationIndexesSeq.size == 0) {
      return Some(new AttackProblemSolution(attackedFacilityIndexes = IndexedSeq.empty[Int].toIndexedSeq,
        objectiveValue = phi0, time = timeUsed(), gap = 0.0, status = "Gap Reached",
        numCuts = nbCuts, numEvaluation = numbEvaluation))
    }

    var ret: Option[AttackProblemSolution] = None

    val cplex = new IloCplex()

    try {
      val attack = Array.tabulate(openLocationIndexesSeq.size)(i => cplex.boolVar("x_"+(i+1)))
      val phi = cplex.numVar(-1E12, 0)

      cplex.addMinimize(phi)
      cplex.setOut(null)
      cplex.setWarning(null)
      cplex.use(new SuperModularCutLazyConstraint(cplex, attack, phi))

//      cplex.setParam(IloCplex.DoubleParam.EpGap, 0)
      cplex.setParam(IloCplex.DoubleParam.TiLim, timeLimit)

      cplex.addEq(attack.fold(cplex.numExpr())(cplex.sum), Math.min(attackUnitsInSubProblem, openLocationIndexes.size))
      
      for( j <- fixedAttack) {
         cplex.addEq(attack(openLocationIndexesSeq.indexOf(j)), 1) 
      }

      if (cplex.solve()) {
        val attackValues = (0 until openLocationIndexesSeq.size).map { j => (openLocationIndexesSeq(j), cplex.getValue(attack(j))) }.toMap

        val attackFacilityIndexes = openLocationIndexesSeq.filter { j => attackValues(j) > 0.5 }

        if (lowerBound > (-cplex.getBestObjValue + phi0)) {
//          println(s"Lower bound updated by the master MIP bestObjValue ${lowerBound} -> ${(-cplex.getBestObjValue + phi0)}")
          lowerBound = (-cplex.getBestObjValue + phi0)
        }

        var finalGap = (lowerBound - upperBound) / upperBound
        if (finalGap < 0) finalGap = 0.0
//        print(finalGap)

//        println(s"Final Upper bound: ${upperBound}, Lower Bound: ${lowerBound}, Gap: ${finalGap}")
        val status = if (timeLimitReached()) "Time Reached" else "Gap Reached"

        ret = Some(new AttackProblemSolution(attackedFacilityIndexes = attackFacilityIndexes.toIndexedSeq,
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

object AttackerProblemCuttingPlaneSolver {
  def main(args: Array[String]): Unit = {
    val outCapture = System.out

    Console.withOut(outCapture) {
      val (demands, dcs) = InstanceReader.readInstanceFrom("input//UCFLData5.txt")

      val p = 3
      val alpha = 1.0
      val theta = 400
      val w = 0.4

      val defenceUnits = 1
      val attackUnits = 1

      val locationInstance = new LocationProblemInstance(demands, dcs)
//      println(demands)
      
      locationInstance.distance.map(row => println(row.toArray.mkString(" ")))
      
//      println(locationInstance.distance)
      
      
      val pMedianInstance = new PMedianProblem(locationInstance, p)
      val defenderInstance = new DefenderProblemInstance(locationInstance, defenceUnits, attackUnits, alpha, theta, w)
      println(defenderInstance.randomFailRate)
      println(defenderInstance.locationContext.demandPoints.map(i => i.demand))
      
      val attackInstance = new AttackerProblemInstance(defenderInstance, (0 until p).toSet, (0 until 1).toSet, attackUnits)

      val instructor = SolverInstructor()

      val solver = new AttackerProblemCuttingPlaneSolver(attackInstance, instructor)

      solver.solve() match {
        case Some(sol) => println(sol)
        case _         => println("Error")
      }
    }

  }
}
