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

object AttackProblemVaryMain {
  def main(args: Array[String]): Unit = {

    val (resultOut, logOut) = ResultOutput.createOutputPrinter("AttackProblemSolverParaVary")

    val outCapture = logOut

    val nodes = List(50, 75, 100, 150)
    val ps = List(15, 20, 30)
    val units = List(3, 6, 9)

    val alpha_thetas = List((0.1, 200), (0.2, 400), (0.3, 800))

    val ws = List(0.4, 0.6, 0.8)

    Console.withOut(outCapture) {

      for (node <- nodes; p <- ps; unit<-units) {
        val (demands, dcs) = InstanceReader.readInstanceFrom(s"input//UCFLData${node}.txt")

        val defenceUnits = 0
        val attackUnits = unit

        val locationInstance = new LocationProblemInstance(demands, dcs)
        val pMedianInstance = new PMedianProblem(locationInstance, p)

        val openLocations = new PMedianSolver(pMedianInstance).solve()

        println(openLocations)

        for ((alpha, theta) <- alpha_thetas) {
          val fortifiedLocations = openLocations.take(openLocations.size / 2)
          resultOut.print(s"${node}\t${p}\t${attackUnits}\t${alpha}\t${theta}\t")
          for (w <- ws) {

            val defenderInstance = new DefenderProblemInstance(locationInstance, defenceUnits, attackUnits, alpha, theta, w)
            val attackInstance = new AttackerProblemInstance(defenderInstance, openLocations, fortifiedLocations, attackUnits)

            val instructor = SolverInstructor(3600, 0.001)

            val solverCuttingPlane = new AttackerProblemCuttingPlaneSolver(attackInstance, instructor)

            solverCuttingPlane.solve() match {
              case Some(sol) => {
                println(sol)
                resultOut.print(s"${sol.objectiveValue}\t${sol.time}\t${sol.numCuts}\t${sol.numEvaluation}")
              }
              case _ => println("Error")
            }

            resultOut.print("\t")
          }
          resultOut.println()
          resultOut.flush()
        }

      }

      resultOut.close()

    }

  }
}