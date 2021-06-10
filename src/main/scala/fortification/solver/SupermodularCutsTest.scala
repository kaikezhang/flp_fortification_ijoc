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
import scala.util.Random
import scala.collection.mutable.Map

class SupermodularCutsTest(val instance: RIMPInstance, val solutuion_pool_size: Int, val total_cuts:Int) extends timer {
  import instance._
  import instance.locationContext._

  val openLocations = openLocationIndexes.toSeq
  val nb_open = openLocations.length
  

  
  def get_random_set_to_fortify():Set[Int] = {
    return openLocations.filter(_ => Random.nextDouble < 0.5).toSet
  }
  
  def generate_random_fortify_solution():Set[Int] = {
    return Random.shuffle(openLocations.toList).take(defenceUnits).toSet  
  }

  val solution_pool = (1 to solutuion_pool_size).map{ i=> generate_random_fortify_solution() }

  def getSolCostswithFortification(fortify:Set[Int]):Double = {
    val attackInstance = new AttackerProblemInstance(instance.locationContext, openLocationIndexes, fortify, attackUnits)
    val solver = new AttackerProblemCuttingPlaneSolver(attackInstance)
    solver.solve().get.objectiveValue
  }
  
  
  val solution_costs = solution_pool.map{ fortify => getSolCostswithFortification(fortify) }
  
  
      
  def generate_cut_for_solution(fortify:Set[Int]):(Double, Map[Int, Double]) = {
    val my_map = Map[Int, Double]()
    val openButNotFortifiedLocations = openLocations.toSet.diff(fortify)
    val solutionTrspCosts = getSolCostswithFortification(fortify)
    
    for(j <- openLocations) {
      my_map += (j -> 0.0)
    }

    for (j <- openButNotFortifiedLocations) {
      val tempFortify = fortify + j
      val trspCostsAfterFortify = getSolCostswithFortification( tempFortify)
      val marginal = trspCostsAfterFortify - solutionTrspCosts
      
      my_map += (j -> marginal)

    }
    
//    println(fortify)
//    
//    println(solutionTrspCosts, my_map)
    
    return (solutionTrspCosts, my_map)
  }
  
  
  val overall_solution_validality =  collection.mutable.IndexedSeq( (1 to solutuion_pool_size).map{ i=> 0 } : _*)

  def cal_pct_solution_cuted( costs: Double,  coeff: Map[Int, Double]):Double = {
    val solution_validality =  collection.mutable.IndexedSeq( (1 to solutuion_pool_size).map{ i=> 0 } : _*)
    
    for( i <- 0 until solutuion_pool_size){
      val sol = solution_pool(i)
      val sol_costs = solution_costs(i)
      val cut_costs = costs + sol.map( j => coeff(j)).sum
      if(sol_costs < cut_costs){
        solution_validality(i) = 1
        overall_solution_validality(i) = 1
      }
    }
    
    return  1.0 * solution_validality.sum / solutuion_pool_size
  }
  
  
  


  def evaluate():(Double, Double) = {

    beginTime = System.currentTimeMillis()
    
    
    var count_pct = 0.0
    

    for( x <- 1 to total_cuts ){
      val sol = generate_random_fortify_solution()
      
      val (costs, coeff) =  generate_cut_for_solution(sol)
      
      count_pct += cal_pct_solution_cuted(costs, coeff)
    }
    
    val invalid_percut = 1.0 * count_pct / total_cuts
    val overal_invalid_pct =  1.0 * overall_solution_validality.sum / solutuion_pool_size
    
    return (invalid_percut, overal_invalid_pct)
  }

}

object SupermodularCutsTest {
  def main(args: Array[String]): Unit = {
    val outCapture = System.out

    Console.withOut(outCapture) {
      val (demands, dcs) = InstanceReader.readInstanceFrom("input//UCFLData50.txt")

      val alpha = 0.2
      val theta = 400
      val w = 0.4
      val p = 15

      val defendUnit = 3 
      val attackUnit =  3
      
      
      val locationInstance = new LocationProblemInstance(demands, dcs)
      val pMedianInstance = new PMedianProblem(locationInstance, p)

      val openLocations = new PMedianSolver(pMedianInstance).solve()

      println(openLocations)

      val defenderInstance = new DefenderProblemInstance(locationInstance, defendUnit, attackUnit, alpha, theta, w)
      val rimpInstance = new RIMPInstance(defenderInstance, openLocations)


      val tester = new SupermodularCutsTest(rimpInstance, 1000, 100)

      print(tester.evaluate())
    }

  }
}