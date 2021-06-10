package fortification.solver

import fortification.data.RIMPInstance
import util.control.Breaks._
import fortification.data.SolverInstructor
import fortification.data.RIMPSolution
import fortification.data.AttackerProblemInstance
import fortification.data.timer
import fortification.data.PMedianProblem
import fortification.data.InstanceReader
import fortification.data.LocationProblemInstance
import fortification.data.DefenderProblemInstance

class RIMPZhuSolver(val instance: RIMPInstance, val instructor: SolverInstructor, attackSolver: String = "CuttingPlane") extends timer {

  import instance._
  import instance.locationContext._

  var numEvaluation = 0
  var numAttackProblem = 0

  def solve(): Option[RIMPSolution] = {

    beginTime = System.currentTimeMillis()
    timeLimit = instructor.timeLimit

    var ret: Option[RIMPSolution] = None

    var solutionSet = Set.empty[Int]

    val q = Math.min(defenceUnits, openLocationIndexes.size)
    breakable {
      if (timeLimitReached())
        break
      for (k <- (1 to q)) {
        val Ses = (1 to attackUnits).map(i => {
          numAttackProblem += 1
          if (timeLimitReached())
            break
          val attackInstance = new AttackerProblemInstance(instance.locationContext, openLocationIndexes, solutionSet, i)
          val solution = if (attackSolver.contains("Cutting"))
            new AttackerProblemCuttingPlaneSolver(attackInstance, instructor).solve().get
          else
            new AttackerProblemEnumerateSolver(attackInstance, instructor).solve().get

          numEvaluation += solution.numEvaluation
          val attackedIndexes = solution.attackedFacilityIndexes
          attackedIndexes
        })

        var candidates = Set.empty[Int]

        val nonFortifiedIndexes = openLocationIndexes.diff(solutionSet)

        val L = nonFortifiedIndexes.map { j =>
          {
            val Lj = Ses.map { S => if (S.contains(j)) 1 else 0 }.sum
            (j, Lj)
          }
        }.toMap

        if (k < defenceUnits - 1) {
          val sorted = nonFortifiedIndexes.toList.sortBy { j => -L(j) }
          candidates = sorted.filter { j => L(j) >= L(sorted.head) }.toSet

        } else if (k == defenceUnits - 1) {
          val sorted = nonFortifiedIndexes.toList.sortBy { j => -L(j) }
          candidates = sorted.take(Math.max(1, attackUnits / 2)).toSet
        } else {
          candidates = nonFortifiedIndexes.intersect(Ses(attackUnits - 1).toSet)
        }

        if (candidates.size > 0) {
          val facilityToAdd = candidates.map { j =>
            {
              numAttackProblem += 1

              val testFortifyIndexes = solutionSet + j
              val attackInstance = new AttackerProblemInstance(instance.locationContext, openLocationIndexes, testFortifyIndexes, attackUnits)
              val solution = if (attackSolver.contains("Cutting"))
                new AttackerProblemCuttingPlaneSolver(attackInstance, instructor).solve().get
              else
                new AttackerProblemEnumerateSolver(attackInstance, instructor).solve().get
              numEvaluation += solution.numEvaluation
              val objectiveValue = solution.objectiveValue
              (j, objectiveValue)
            }
          }.minBy(_._2)._1

          solutionSet = solutionSet + facilityToAdd
        }

      }

      numAttackProblem += 1

      val attackInstance = new AttackerProblemInstance(instance.locationContext, openLocationIndexes, solutionSet, attackUnits)
      val solution = if (attackSolver.contains("Cutting"))
        new AttackerProblemCuttingPlaneSolver(attackInstance, instructor).solve().get
      else
        new AttackerProblemEnumerateSolver(attackInstance, instructor).solve().get
      numEvaluation += solution.numEvaluation

      ret = Option(new RIMPSolution(fortifiedIndexes = solutionSet.toIndexedSeq,
        attackedFacilityIndexes = solution.attackedFacilityIndexes,
        objectiveValue = solution.objectiveValue, time = timeUsed(), gap = 0, status = "Heuristic",
        numCallsToAttackProblem = numAttackProblem, numEvaluation = numEvaluation))

    }
    
    if(timeLimitReached()) {
      ret = Option(new RIMPSolution(fortifiedIndexes = solutionSet.toIndexedSeq,
        attackedFacilityIndexes = IndexedSeq.empty[Int],
        objectiveValue = 0, time = timeUsed(), gap = 0, status = "Timelimit reached",
        numCallsToAttackProblem = numAttackProblem, numEvaluation = numEvaluation))      
    }

    ret
  }
}

object RIMPZhuSolver {
  def main(args: Array[String]): Unit = {
    val outCapture = System.out

    Console.withOut(outCapture) {
      val (demands, dcs) = InstanceReader.readInstanceFrom("input//UCFLData50.txt")

      val p = 20
      val alpha = 2.0
      val theta = 400
      val w = 0.4

      val defenceUnits = 3
      val attackUnits = 6

      val locationInstance = new LocationProblemInstance(demands, dcs)
      val pMedianInstance = new PMedianProblem(locationInstance, p)

      val openLocations = new PMedianSolver(pMedianInstance).solve()

      val defenderInstance = new DefenderProblemInstance(locationInstance, defenceUnits, attackUnits, alpha, theta, w)

      val rimpInstance = new RIMPInstance(defenderInstance, openLocations)

      val instructor = SolverInstructor(3600, 0.0)

      val solverEnu = new RIMPZhuSolver(rimpInstance, instructor, "Enumerate")

      solverEnu.solve() match {
        case Some(sol) => println(sol)
        case _         => println("Error")
      }

      val solverCutt = new RIMPZhuSolver(rimpInstance, instructor, "Cutting")

      solverCutt.solve() match {
        case Some(sol) => println(sol)
        case _         => println("Error")
      }
    }

  }
}

