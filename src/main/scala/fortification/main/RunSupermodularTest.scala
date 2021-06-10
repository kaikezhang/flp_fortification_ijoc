package fortification.main

import fortification.solver.AttackerProblemCuttingPlaneSolver
import fortification.data.DefenderProblemInstance
import fortification.data.DefenderProblemInstanceP0
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
import fortification.solver.RIMPCuttingPlaneSolver
import fortification.solver.RIMPImplicitEnumerateSolver
import fortification.solver.SupermodularCutsTest

object SupermodularCutViolationTest {
  
  def main(args: Array[String]): Unit = {

    val (resultOut, logOut) = ResultOutput.createOutputPrinter("SupermodularCutTest")

    val outCapture = logOut

    val nodes = List(50, 75, 100, 150)
    val ps = List(15, 20, 30)
    
    val defendUnits = List(3, 6, 9)
    val attackUnits = List(3, 6, 9)

//    val alpha_thetas = List((0.1, 200), (0.2, 400), (0.3, 800))
//
//    val ws = List(0.4, 0.6, 0.8)
      val alpha = 0.2
      val theta = 400
      val w = 0.4
      
      
        
    Console.withOut(outCapture) {

      for (node <- nodes; p <- ps; defendUnit <- defendUnits; attackUnit <- attackUnits) {
        val (demands, dcs) = InstanceReader.readInstanceFrom(s"input//UCFLData${node}.txt")


        val locationInstance = new LocationProblemInstance(demands, dcs)
        val pMedianInstance = new PMedianProblem(locationInstance, p)

        val openLocations = new PMedianSolver(pMedianInstance).solve()

//        for ((alpha, theta) <- alpha_thetas) {
          
            resultOut.print(s"${node},${p},${defendUnit},${attackUnit},")
//            for (w <- ws) {
            
                val defenderInstanceP0 = new DefenderProblemInstanceP0(locationInstance, defendUnit, attackUnit, w)
                val rimpInstanceP0 = new RIMPInstance(defenderInstanceP0, openLocations)
                val testerP0 = new SupermodularCutsTest(rimpInstanceP0, 1000, 100)
                val retP0 = testerP0.evaluate()
                resultOut.print(s"${retP0._1},${retP0._2},")
                
  
                val defenderInstance = new DefenderProblemInstance(locationInstance, defendUnit, attackUnit, alpha, theta, w)
                val rimpInstance = new RIMPInstance(defenderInstance, openLocations) 
                val tester = new SupermodularCutsTest(rimpInstance, 1000, 100)      
                val ret = tester.evaluate()               
                resultOut.print(s"${ret._1},${ret._2}")
                
                
       
//            }
          resultOut.println()
          resultOut.flush()
//        }

      }

      resultOut.close()

    }

  }
}