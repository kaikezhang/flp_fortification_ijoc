# flp_fortification
 
Data used in this paper can be found in `input` folder.
Each file corresponding to an instance with specified number of cities.
The first line is the number of cities `n`.
Next `n` lines, each line specifies a city in the order of:
 - index (int)
 - demand (float)
 - emergency cost (float)
 - fixed facility cost (float)
 - latitude (float)
 - longitude (float)

Code is written in Scala and can be run with Scala IDEs, no addition package needs to be installed, but you may need to configure path so CPLEX can be recognized. Result tables are gathered by run corresponding files.

Table 3
 - AttackProblemCompareMain.scala
 - AttackProblemCompareMainLinear.scala

Table 4, Table 5 
 - AttackProblemVaryMain.scala

Table 6, Table 7
 - CompareRIMPSolverZhu.scala
 - RIMPSolverCuttingPlane.scala
 - RIMPSolverTreeSearch.scala
  
Table 8, Table 9
 - CompareRIMPSolversP0.scala
