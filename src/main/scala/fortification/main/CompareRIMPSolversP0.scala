package fortification.main

import fortification.solver.AttackerProblemCuttingPlaneSolver
import fortification.data.DefenderProblemInstanceP0
import fortification.solver.AttackerProblemEnumerateSolver
import fortification.solver.RIMPCuttingPlaneSolver
import fortification.data.output.ResultOutput
import fortification.data.InstanceReader
import fortification.data.AttackerProblemInstance
import fortification.data.SolverInstructor
import fortification.solver.PMedianSolver
import fortification.data.PMedianProblem
import fortification.data.LocationProblemInstance
import fortification.data.RIMPInstance
import fortification.solver.RIMPZhuSolver
import fortification.solver.RIMPImplicitEnumerateSolver

object CompareRIMPSolversP0 {
  def main(args: Array[String]): Unit = {

    val (resultOut, logOut) = ResultOutput.createOutputPrinter("RIMPZhuSolversCompareP0W0")

    val outCapture = logOut

    val nodes = List(50, 75)
    val ps = List(15, 20, 30)
    val defendUnits = List(3, 6, 9)
    val attackUnits = List(3, 6, 9)

    Console.withOut(outCapture) {

      for (node <- nodes; p <- ps; defendUnit <- defendUnits; attackUnit <- attackUnits) {
        val (demands, dcs) = InstanceReader.readInstanceFrom(s"input//UCFLData${node}.txt")

        val w = 0.0

        resultOut.print(s"${node},${p},${defendUnit},${attackUnit},")

        val locationInstance = new LocationProblemInstance(demands, dcs)
        val pMedianInstance = new PMedianProblem(locationInstance, p)

        val openLocations = new PMedianSolver(pMedianInstance).solve()

        val defenderInstance = new DefenderProblemInstanceP0(locationInstance, defendUnit, attackUnit, w)

        val rimpInstance = new RIMPInstance(defenderInstance, openLocations)

        val instructor = SolverInstructor(3600, 0.0001)

        val solverCuttingPlane = new RIMPCuttingPlaneSolver(rimpInstance, instructor)

        val solverEnumerate = new RIMPZhuSolver(rimpInstance, instructor, "Cutting")
        
        val implicitEM = new RIMPImplicitEnumerateSolver(rimpInstance, instructor)

        implicitEM.solve() match {
          case Some(sol) => {
            println(sol)
            resultOut.print(s"${sol.objectiveValue},${sol.time},${sol.numEvaluation},${sol.numCallsToAttackProblem}")
          }
          case _ => print("Error")
        }
        
        resultOut.print(",")
        
        solverEnumerate.solve() match {
          case Some(sol) => {
            println(sol)
            resultOut.print(s"${sol.objectiveValue},${sol.time},${sol.numEvaluation},${sol.numCallsToAttackProblem}")
          }
          case _ => print("Error")
        }
        
        resultOut.print(",")
        
        solverCuttingPlane.solve() match {
          case Some(sol) => {
            println(sol)
            resultOut.print(s"${sol.objectiveValue},${sol.time},${sol.numEvaluation},${sol.numCallsToAttackProblem}")
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