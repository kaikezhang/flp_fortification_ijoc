package fortification.solver

import ilog.cplex.IloCplex
import ilog.cplex.CpxException
import fortification.data.SolverInstructor
import scala.util.control.NonFatal
import fortification.data.PMedianProblem
import fortification.data.InstanceReader
import fortification.data.LocationProblemInstance
import fortification.data.AttackerProblemInstance
import fortification.data.DefenderProblemInstance


class PMedianSolver(instance: PMedianProblem){

  import instance._
  import instance.locationContext._
  
  def solve(): Set[Int] = {
    
    val cplex = new IloCplex()
    
    var ret = Set.empty[Int]

    try {
      val open = Array.tabulate(candidateLocations.size)( i => cplex.boolVar() )
      val assign = Array.tabulate(demandPoints.size, candidateLocations.size)((i, j) => {cplex.boolVar()})
     
      val transportationCosts = (for (i <- demandsPointIndexes; j <- candidateLocationIndexes)
        yield cplex.prod(demandPoints(i).demand * distance(i)(j), assign(i)(j))).fold(cplex.numExpr())(cplex.sum)

  
      cplex.addMinimize(transportationCosts)
      
      cplex.setOut(null)
      
      demandsPointIndexes.foreach { i => {
        val left = candidateLocationIndexes.map { j => assign(i)(j) }.fold(cplex.numExpr())(cplex.sum)
        cplex.addGe(left, 1)
      } }
      
      for(i <- demandsPointIndexes; j <- candidateLocationIndexes ) {
        cplex.addLe(assign(i)(j), open(j)) 
      }
      
      cplex.addEq(open.fold(cplex.numExpr())(cplex.sum), instance.p)

      if (cplex.solve()) {
        ret = candidateLocationIndexes.filter { j => cplex.getValue(open(j)) > 0.5 }.toSet
      }

    } catch {
      case e: CpxException => println("Cplex exception caught: " + e);
      case NonFatal(e)     => println("exception caught: " + e);
    } finally {
      cplex.end()
    }
    ret
  }
}

object PMedianSolver {
   def main(args: Array[String]): Unit = {
      val (demands, dcs) = InstanceReader.readInstanceFrom("input//UCFLData50.txt")

      val p = 30
      val alpha = 1.0
      val theta = 400
      val w = 1

      val defenceUnits = 0
      val attackUnits = 5

      val locationInstance = new LocationProblemInstance(demands, dcs)
      val pMedianInstance = new PMedianProblem(locationInstance, p)

      val solver = new PMedianSolver(pMedianInstance)

      println(solver.solve())
   }
}