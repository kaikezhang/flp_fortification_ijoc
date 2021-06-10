package fortification.main

import fortification.solver.AttackerProblemCuttingPlaneSolver
import fortification.data.DefenderProblemInstanceP0
import fortification.data.DefenderProblemInstance
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


object CompareBenifitofRIMP {
  def main(args: Array[String]): Unit = {

    val (resultOut, logOut) = ResultOutput.createOutputPrinter("CompareBenifitofRIMP")

    val outCapture = logOut

    val nodes = List(50)
    val ps = List(20)
    val defendUnits = List(9)
    val attackUnits = List(9)
//    val alphas = List(0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
//    val thetas = List(100, 150, 200, 250, 300, 350, 400, 450, 500)
    
    val alphas = List(0.5)
    val thetas = List(500)
    
    Console.withOut(outCapture) {

      for (node <- nodes; p <- ps; defendUnit <- defendUnits; attackUnit <- attackUnits; alpha <- alphas; theta <- thetas) {
        val (demands, dcs) = InstanceReader.readInstanceFrom(s"input//UCFLData${node}.txt")

        val w = 0.8

        resultOut.print(s"${node},${p},${defendUnit},${attackUnit},${alpha},${theta},")

        val locationInstance = new LocationProblemInstance(demands, dcs)
        val pMedianInstance = new PMedianProblem(locationInstance, p)

        val openLocations = new PMedianSolver(pMedianInstance).solve()

        val defenderInstance = new DefenderProblemInstance(locationInstance, defendUnit, attackUnit, alpha, theta, w)
        
        val defenderInstanceP0 = new DefenderProblemInstanceP0(locationInstance, defendUnit, attackUnit, w)

        val rimpInstance = new RIMPInstance(defenderInstance, openLocations)
        val rimpInstanceP0 = new RIMPInstance(defenderInstanceP0, openLocations)

        val instructor = SolverInstructor(3600, 0.0001)

        val solverCuttingPlane = new RIMPCuttingPlaneSolver(rimpInstance, instructor)
        
        val solverCuttingPlaneP0 = new RIMPCuttingPlaneSolver(rimpInstanceP0, instructor)
        
        var obj_1 = 0.0

        
        solverCuttingPlane.solve() match {
          case Some(sol) => {
            println(sol)
            obj_1 = sol.objectiveValue
            resultOut.print(s"${sol.time}, ${sol.objectiveValue},")
          }
          case _ => print("Error")
        }
        
        solverCuttingPlaneP0.solve() match {
          case Some(sol) => {
            println(sol)
            
            val fortified_solutionP0 = sol.fortifiedIndexes
            
            val (solutionTrspCosts, attacked) = solverCuttingPlane.getTransptCosts(openLocations, fortified_solutionP0.toSet)
             
            resultOut.print(s"${sol.objectiveValue}, ${solutionTrspCosts}, ${ (solutionTrspCosts-obj_1) / obj_1 }")
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