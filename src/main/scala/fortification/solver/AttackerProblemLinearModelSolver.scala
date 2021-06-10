package fortification.solver

import fortification.data.timer
import fortification.data.AttackerProblemInstance
import fortification.data.AttackProblemSolution
import fortification.data.SolverInstructor
import fortification.data.PMedianProblem
import fortification.data.InstanceReader
import fortification.data.LocationProblemInstance
import fortification.data.DefenderProblemInstance
import ilog.cplex.IloCplex
import ilog.cplex.CpxException
import scala.util.control.NonFatal



class AttackerProblemLinearModelSolver(val instance: AttackerProblemInstance, val instructor: SolverInstructor = new SolverInstructor(3600, 0.005))
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
    
    val demands = demandPoints
    
    val c_jv = demandsPointIndexes.map { i =>
      {
        orderedOpenDCs(i).map(j => demands(i).demand * distance(i)(j) )
      }
    }

    val phi0 = getTransptCosts(instance.locationContext, orderedOpenDCs, openLocationIndexes, fortifiedLocationIndexes, Set.empty[Int])

    val openLocationIndexesSeq = openLocationIndexes.toIndexedSeq
    
    if (openLocationIndexesSeq.size == 0) {
      return Some(new AttackProblemSolution(attackedFacilityIndexes = IndexedSeq.empty[Int].toIndexedSeq,
        objectiveValue = phi0, time = timeUsed(), gap = 0.0, status = "Gap Reached",
        numCuts = 0, numEvaluation = 0))
    }
    
    val qj = randomFailRate
    
    val wj = openLocationIndexes.map { j =>
      { 
        var wj = 0.0
        if (fortifiedLocationIndexes(j)) {
          wj =  w - qj(j)*w 
        } else {       
          wj = 1 - qj(j)
        }
        (j, wj)
      }
    }.toMap
    
    val open_loaction_range = 0 until openLocationIndexesSeq.size
    
   
    var ret: Option[AttackProblemSolution] = None

    val cplex = new IloCplex()

    try {
      val attack = Array.tabulate(openLocationIndexesSeq.size)(i => cplex.boolVar())
      
      val theta = Array.tabulate(demandsPointIndexes.size)(i => 
        Array.tabulate(openLocationIndexesSeq.size)(i => cplex.numVar(0.0, 1.0)) )
        
      val total_cost = demandsPointIndexes.map(i 
          => open_loaction_range.map(v 
              =>{
                var ret = cplex.numExpr()
                if(v == 0) {
                 ret = cplex.sum(
                  c_jv(i)(v)* 1,
                  cplex.prod( -c_jv(i)(v), theta(i)(v)))                      
                } else {
                 ret = cplex.sum(
                  cplex.prod(c_jv(i)(v), theta(i)(v-1)),
                  cplex.prod( -c_jv(i)(v), theta(i)(v)))                    
                }
                ret
              }
                ).fold(cplex.numExpr())(cplex.sum)
          ).fold(cplex.numExpr())(cplex.sum)

      cplex.addMaximize(total_cost)
      cplex.setOut(null)
      cplex.setWarning(null)


      cplex.setParam(IloCplex.DoubleParam.EpGap, 0)
      cplex.setParam(IloCplex.DoubleParam.EpAGap, 0.0)
      cplex.setParam(IloCplex.DoubleParam.TiLim, timeLimit)

      cplex.addEq(attack.fold(cplex.numExpr())(cplex.sum), Math.min(attackUnitsInSubProblem, openLocationIndexes.size))
      
      val M = 1.0
      
      for( i <- demandsPointIndexes){
        for( v <- open_loaction_range) {
          val index_v_closest = openLocationIndexesSeq.indexOf( orderedOpenDCs(i)(v) )
          
          if( v == 0){
            cplex.addLe(theta(i)(v),  
                cplex.sum( qj(orderedOpenDCs(i)(v)) + wj(orderedOpenDCs(i)(v)),
                           cplex.prod(1, cplex.diff(1, attack(index_v_closest))) 
                          )
                       )              
            
          } else {
            
            cplex.addLe(theta(i)(v),  
                cplex.sum( cplex.prod( theta(i)(v-1), qj(orderedOpenDCs(i)(v)) ) ,
                           cplex.prod( theta(i)(v-1), wj(orderedOpenDCs(i)(v)) ) ,
                           cplex.prod(M, cplex.diff(1, attack(index_v_closest))) 
//                           cplex.prod(M, cplex.diff(1, attack( orderedOpenDCs(i)(v) ))) 
                          )
                       )            
          }

        }
      }
      
      for( i <- demandsPointIndexes){
        for( v <- open_loaction_range) {
          val index_v_closest = openLocationIndexesSeq.indexOf( orderedOpenDCs(i)(v) )
          
          if(v == 0){
            cplex.addLe(theta(i)(v),  
                cplex.sum( qj(orderedOpenDCs(i)(v)) ,
                           cplex.prod(M, attack(index_v_closest)) 
                         )
                       )              
          } else {
            cplex.addLe(theta(i)(v),  
                cplex.sum( cplex.prod( theta(i)(v-1), qj(orderedOpenDCs(i)(v)) ),
                           cplex.prod(M, attack(index_v_closest)) 
                         )
                       )            
          }

        }
      }
      
      if (cplex.solve()) {
        val attackValues = open_loaction_range.map { j => (openLocationIndexesSeq(j), cplex.getValue(attack(j))) }.toMap

        val attackFacilityIndexes = openLocationIndexesSeq.filter { j => attackValues(j) > 0.5 }

        ret = Some(new AttackProblemSolution(attackedFacilityIndexes = attackFacilityIndexes.toIndexedSeq,
          objectiveValue = cplex.getObjValue, time = timeUsed(), gap = 0.0, status = cplex.getStatus.toString(),
          numCuts = 0, numEvaluation = 0))
      } else {
        println(cplex.getStatus)
      }

    } catch {
      case e: CpxException => println("Cplex exception caught: " + e);
      case NonFatal(e)     => println("exception caught: " + e.printStackTrace());
    } finally {
      cplex.end()
    }
    ret

  }

  

  
}

object AttackerProblemLinearModelSolver{
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

        val solver = new AttackerProblemLinearModelSolver(attackInstance, instructor)
        
        solver.solve() match {
              case Some(sol) => println(sol)
              case _         => println("Error")
        }        
    }
      
  }  
}


//(5, 14, 1, 28, 13)  objective:757452.7815569789    time:1.67 numEvaluated:0 numCuts:0  status:Optimal

//(0, 18, 11, 19, 4)  objective:757452.7507259389    time:0.934 numEvaluated:7415 numCuts:239  status:Gap Reached

//(5, 10, 1, 13, 8)  objective:757452.7507259389    time:9.737 numEvaluated:142506 numCuts:0  status:All Combinations Enumerated



