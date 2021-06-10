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

class RandomInstanceSupermodularTest(val instance: RIMPInstance, val total_exp_count:Int) extends timer {
  import instance._
  import instance.locationContext._

  val openLocations = openLocationIndexes.toSeq
  val nb_open = openLocations.length
  
  def get_random_set_to_fortify():Set[Int] = {
    return openLocations.filter(_ => Random.nextDouble < 0.5).toSet
  }
  
  def getSolCostswithFortification(fortify:Set[Int]):Double = {
    val attackInstance = new AttackerProblemInstance(instance.locationContext, openLocationIndexes, fortify, attackUnits)
    val solver = new AttackerProblemCuttingPlaneSolver(attackInstance)
    solver.solve().get.objectiveValue
  }

  def evaluate():Double = {

    beginTime = System.currentTimeMillis()
    
    var x = 0
    
    var total_true_count = 0
//    var total_exp_count = 1000
    
    for( x <- 1 to total_exp_count ){
      val set_A = get_random_set_to_fortify()
      val set_B = get_random_set_to_fortify()
      val set_A_union_B = set_A.union(set_B)
      val set_A_interset_B = set_A.intersect(set_B)
      
//      println(set_A)
//      println(set_B)
//      println(set_A_union_B)
//      println(set_A_interset_B)
      
      val A_costs = getSolCostswithFortification(set_A)
      val B_costs = getSolCostswithFortification(set_B)
      val A_union_B_costs = getSolCostswithFortification(set_A_union_B)
      val A_interset_B_costs = getSolCostswithFortification(set_A_interset_B)
      
      if (A_costs +  B_costs >= A_union_B_costs + A_interset_B_costs){
        total_true_count = total_true_count + 1
      }

    }
    
    val pct_true = 1.0 * total_true_count / total_exp_count
//    println(pct_true)
    
    return pct_true
    

    
    

  }

}

object RandomInstanceSupermodularTest {
  def main(args: Array[String]): Unit = {
    val outCapture = System.out

    Console.withOut(outCapture) {
      val (demands, dcs) = InstanceReader.readInstanceFrom("input//UCFLData50.txt")

      val alpha = 0.2
      val theta = 400
      val w = 0.4
      val p = 15

      val defendUnit = 0 
      val attackUnit =  3
      
      val locationInstance = new LocationProblemInstance(demands, dcs)
      val pMedianInstance = new PMedianProblem(locationInstance, p)

      val openLocations = new PMedianSolver(pMedianInstance).solve()

      println(openLocations)

      val defenderInstance = new DefenderProblemInstance(locationInstance, defendUnit, attackUnit, alpha, theta, w)
      val rimpInstance = new RIMPInstance(defenderInstance, openLocations)


      val tester = new RandomInstanceSupermodularTest(rimpInstance, 1)

      tester.evaluate()
    }

  }
}