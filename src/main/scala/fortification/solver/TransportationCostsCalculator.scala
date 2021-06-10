package fortification.solver

import scala.util.Random
import fortification.data.DefenderProblemInstance
import scala.collection.immutable.TreeSet

trait TransportationCostsCalculator {
  def getTransptCosts(locationContext: DefenderProblemInstance,
                      openLocationIndexes: Set[Int],
                      fortifiedLocationIndexes: Set[Int],
                      attackLocationIndexes: Set[Int]): Double = {
    val w = locationContext.w
    val randomFailRate = locationContext.randomFailRate
    val demandIndexes = locationContext.locationContext.demandsPointIndexes
    val demands = locationContext.locationContext.demandPoints
    
    val distance = locationContext.locationContext.distance
    
    val p = openLocationIndexes.map { j =>
      {
        var s = 0.0
        var z = 0.0
        val q = randomFailRate(j)
        if (attackLocationIndexes(j))
          s = 1.0
        if (fortifiedLocationIndexes(j)) {
          z = 1.0
        }
        (j, 1.0 - (1.0- q)*(1.0 - s* Math.pow(w, z)))
      }
    }.toMap
    
    val orderedOpenDCs = demandIndexes.map { i =>
      {
        openLocationIndexes.toSeq.sortBy { j => distance(i)(j) }
      }
    }
    
    demandIndexes.map { i =>
      {
        var rate = 1.0
        var costs = 0.0

        orderedOpenDCs(i).foreach { j =>
          {
            costs += rate * (1 - p(j)) * demands(i).demand * distance(i)(j)
            rate = rate * p(j)
          }
        }
        costs += rate * demands(i).emergencyCost * demands(i).demand
        costs
      }
     }.sum     
  }
  
  
  def getTransptCosts(locationContext: DefenderProblemInstance,
                      orderedOpenDCs: IndexedSeq[Seq[Int]],
                      openLocationIndexes: Set[Int],
                      fortifiedLocationIndexes: Set[Int],
                      attackLocationIndexes: Set[Int]): Double = {
    val w = locationContext.w
//    print(w)
    val randomFailRate = locationContext.randomFailRate
    val demandIndexes = locationContext.locationContext.demandsPointIndexes
    val demands = locationContext.locationContext.demandPoints
    
    val distance = locationContext.locationContext.distance

    val p = openLocationIndexes.map { j =>
      {
        var s = 0.0
        var z = 0.0
        val q = randomFailRate(j)
        if (attackLocationIndexes(j))
          s = 1.0
        if (fortifiedLocationIndexes(j)) {
          z = 1.0
        }
        (j, 1.0 - (1.0- q)*(1.0 - s* Math.pow(w, z)))
      }
    }.toMap
    
    
    demandIndexes.map { i =>
      {
        var rate = 1.0
        var costs = 0.0

        orderedOpenDCs(i).foreach { j =>
          {
            costs += rate * (1 - p(j)) * demands(i).demand * distance(i)(j)
            rate = rate * p(j)
          }
        }
        costs += rate * demands(i).emergencyCost * demands(i).demand
        costs
      }
     }.sum     
  }
  
  
}