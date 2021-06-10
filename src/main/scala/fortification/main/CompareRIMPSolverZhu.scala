package fortification.main

import fortification.solver.AttackerProblemCuttingPlaneSolver
import fortification.data.DefenderProblemInstance
import fortification.solver.AttackerProblemEnumerateSolver
import fortification.data.output.ResultOutput
import fortification.data.InstanceReader
import fortification.data.AttackerProblemInstance
import fortification.data.SolverInstructor
import fortification.solver.PMedianSolver
import fortification.data.PMedianProblem
import fortification.data.LocationProblemInstance
import fortification.data.RIMPInstance
import fortification.solver.RIMPZhuSolver

object CompareRIMPSolverZhu {
  def main(args: Array[String]): Unit = {

    val (resultOut, logOut) = ResultOutput.createOutputPrinter("RIMPZhuSolverCompare")

    val outCapture = logOut

    val nodes = List(50, 75, 100, 150)
    val ps = List(15, 20, 30)
    val defendUnits = List(3, 6, 9)
    val attackUnits = List(3, 6, 9)

    Console.withOut(outCapture) {

      for (node <- nodes; p <- ps; defendUnit <- defendUnits; attackUnit <- attackUnits) {
        val (demands, dcs) = InstanceReader.readInstanceFrom(s"input//UCFLData${node}.txt")
        val alpha = 0.2
        val theta = 400
        val w = 0.4

        resultOut.print(s"${node}\t${p}\t${defendUnit}\t${attackUnit}\t")

        val locationInstance = new LocationProblemInstance(demands, dcs)
        val pMedianInstance = new PMedianProblem(locationInstance, p)

        val openLocations = new PMedianSolver(pMedianInstance).solve()

//        println(openLocations)

        val defenderInstance = new DefenderProblemInstance(locationInstance, defendUnit, attackUnit, alpha, theta, w)

        val rimpInstance = new RIMPInstance(defenderInstance, openLocations)

        val instructor = SolverInstructor(3600, 0.0001)

        val solverCuttingPlane = new RIMPZhuSolver(rimpInstance, instructor, "Cutting")

        val solverEnumerate = new RIMPZhuSolver(rimpInstance, instructor, "Enumerate")

        solverCuttingPlane.solve() match {
          case Some(sol) => {
            println(sol)
            resultOut.print(s"${sol.objectiveValue}\t${sol.time}\t${sol.numEvaluation}\t${sol.numCallsToAttackProblem}")
          }
          case _ => print("Error")
        }

        resultOut.print("\t")

        solverEnumerate.solve() match {
          case Some(sol) => {
            println(sol)
            resultOut.print(s"${sol.objectiveValue}\t${sol.time}\t${sol.numEvaluation}\t${sol.numCallsToAttackProblem}")
          }
          case _ => print("Error")
        }

        resultOut.println()
        resultOut.flush()

      }

      resultOut.close()

    }

  }
}