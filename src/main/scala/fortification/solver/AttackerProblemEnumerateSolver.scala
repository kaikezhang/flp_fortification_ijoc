package fortification.solver

import fortification.data.timer
import fortification.data.AttackerProblemInstance
import fortification.data.AttackProblemSolution
import fortification.data.SolverInstructor
import fortification.data.PMedianProblem
import fortification.data.InstanceReader
import fortification.data.LocationProblemInstance
import fortification.data.DefenderProblemInstance



class AttackerProblemEnumerateSolver(val instance: AttackerProblemInstance, val instructor: SolverInstructor = new SolverInstructor(3600, 0.005))
                                extends timer with TransportationCostsCalculator  {
  import instance._
  import instance.locationContext._
  import instance.locationContext.locationContext._  
  
  def solve():Option[AttackProblemSolution] = {
 
    beginTime = System.currentTimeMillis()
    timeLimit = instructor.timeLimit
    
    val orderedOpenDCs = demandsPointIndexes.map { i =>
      {
        openLocationIndexes.toSeq.sortBy { j => distance(i)(j) }
      }
    }    
    
    val combinations = openLocationIndexes.toList.combinations(Math.min(openLocationIndexes.size, attackUnitsInSubProblem))
    
    var numbEvaluation = 0
    
    var bestSol = Set.empty[Int]
    var bestCosts = getTransptCosts(instance.locationContext, orderedOpenDCs, openLocationIndexes, fortifiedLocationIndexes, bestSol)


           
    
    while(combinations.hasNext && !timeLimitReached){
      numbEvaluation += 1
      val sol = combinations.next().toSet
   
      val costs = getTransptCosts(instance.locationContext, orderedOpenDCs, openLocationIndexes, fortifiedLocationIndexes, sol)
      
//      println(sol.mkString("(", ", ", ")") +"--->" + costs ) 
      if(bestCosts < costs) {
        bestCosts =  costs
        bestSol = sol
      }
    }
    
    val status = if( timeLimitReached()) "Time Reached" else  "All Combinations Enumerated"
    Some(new AttackProblemSolution(attackedFacilityIndexes = bestSol.toIndexedSeq,  
                                   objectiveValue = bestCosts,
                                   time = timeUsed(),  
                                   gap = 0.0, 
                                   status = status, 
                                   numEvaluation = numbEvaluation))  

  }

  

  
}

object AttackerProblemEnumerateSolver{
  def main(args: Array[String]): Unit = {
    val outCapture = System.out
    
    Console.withOut(outCapture) {
        val (demands, dcs) = InstanceReader.readInstanceFrom("input//UCFLData50.txt")
        
        val p = 30
        val alpha = 1.0
        val theta = 400
        val w = 0.4
        
        val defenceUnits = 0
        val attackUnits = 5
        
        val locationInstance =  new LocationProblemInstance(demands, dcs)
        val pMedianInstance =  new PMedianProblem(locationInstance, p)
        val defenderInstance = new DefenderProblemInstance(locationInstance, defenceUnits, attackUnits, alpha, theta, w)
        val attackInstance = new AttackerProblemInstance(defenderInstance, (0 until p).toSet, Set.empty[Int], attackUnits)
        
        val instructor = SolverInstructor()

        val solver = new AttackerProblemEnumerateSolver(attackInstance, instructor)
        
        solver.solve() match {
              case Some(sol) => println(sol)
              case _         => println("Error")
        }        
    }
      
  }  
}




