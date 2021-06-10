package fortification.solver

import fortification.data.timer
import util.control.Breaks._
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

class RIMPImplicitEnumerateSolver(val instance: RIMPInstance, val instructor: SolverInstructor) extends timer {
  import instance._
  import instance.locationContext._
  import instance.locationContext.locationContext._

  class IENode(val fixedFortified: Set[Int], val fixedNotFortified: Set[Int], val candidates: Set[Int]) {
    override def toString() = {
      s"Fortified:${fixedFortified} noFortified:${fixedNotFortified} I=${candidates}"
    }
  }

  var upperBound = Double.MaxValue

  var bestSolution = Set.empty[Int]
  var bestAttacked = Set.empty[Int]

  var numbEvaluation = 0
  var numAttackProblem = 0

  val openIndexes = openLocationIndexes.toIndexedSeq

  def getTransptCosts(fortifyLocs: Set[Int]) = {

    numAttackProblem += 1

    val attackInstance = new AttackerProblemInstance(instance.locationContext, openLocationIndexes, fortifyLocs, attackUnits)

    val instructor = SolverInstructor(3600, 0.0001)

    val solution = new AttackerProblemCuttingPlaneSolver(attackInstance, instructor).solve().get

    numbEvaluation += solution.numEvaluation
    (solution.objectiveValue, solution.attackedFacilityIndexes.toSet)

  }

  def solve(): Option[RIMPSolution] = {

    beginTime = System.currentTimeMillis()
    timeLimit = instructor.timeLimit

    var ret: Option[RIMPSolution] = None

    val (obj, attacked) = getTransptCosts(Set.empty[Int])

    val root = new IENode(Set.empty[Int], Set.empty[Int], attacked)

    val stack = scala.collection.mutable.Stack.empty[IENode]

    stack.push(root)

    while (!stack.isEmpty && !timeLimitReached()) {

      val node = stack.pop()
            println(node)
      if (node.fixedFortified.size < defenceUnits) {

        if (node.candidates.size > 0) {
          val indexToBranch = node.candidates.head

          val leftFortified = node.fixedFortified + indexToBranch
          val (obj, attackedLeft) = getTransptCosts(leftFortified)
          val candidatesLeft = attackedLeft.diff(node.fixedNotFortified).diff(leftFortified)
          val leftNode = new IENode(leftFortified, node.fixedNotFortified, candidatesLeft)
          
            if (obj < upperBound) {
              upperBound = obj
              bestSolution = leftFortified
              bestAttacked = attackedLeft
            }          

          val rightNotFortified = node.fixedNotFortified + indexToBranch
          val candidatesRight = node.candidates.diff(Set(indexToBranch))
          val rightNode = new IENode(node.fixedFortified, rightNotFortified, candidatesRight)

          stack.push(rightNode)

          stack.push(leftNode)
        }

      } else {
        val (obj, attack) = getTransptCosts(node.fixedFortified)
        if (obj < upperBound) {
          upperBound = obj
          bestSolution = node.fixedFortified
          bestAttacked = attack
        }
      }

    }

    ret = Some(new RIMPSolution(
      fortifiedIndexes = bestSolution.toIndexedSeq,
      attackedFacilityIndexes = bestAttacked.toIndexedSeq,
      objectiveValue = upperBound, time = timeUsed(), gap = 0.0, status = "Optimal-enumerated",
      numCallsToAttackProblem = numAttackProblem, numEvaluation = numbEvaluation))

    ret
  }

}

object RIMPImplicitEnumerateSolver {
  def main(args: Array[String]): Unit = {
    val outCapture = System.out

    Console.withOut(outCapture) {
      val (demands, dcs) = InstanceReader.readInstanceFrom("input//UCFLData50.txt")

      val alpha = 0.2
      val theta = 400
      val w = 0.4

      val p = 20

      val defenceUnits = 9
      val attackUnits = 3

      val locationInstance = new LocationProblemInstance(demands, dcs)

      val pMedianInstance = new PMedianProblem(locationInstance, p)

      val openLocations = new PMedianSolver(pMedianInstance).solve()

      val defenderInstance = new DefenderProblemInstance(locationInstance, defenceUnits, attackUnits, alpha, theta, w)

      val rimpInstance = new RIMPInstance(defenderInstance, openLocations)

      val instructor = SolverInstructor(gap = 0.0001)

      val solver = new RIMPImplicitEnumerateSolver(rimpInstance, instructor)

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