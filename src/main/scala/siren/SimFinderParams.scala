/*

Author:
    Kristal Curtis

*/

package siren

abstract class SimFinderParams(seedLenStr: String, readLenStr: String, numSeedsStr: String, unionDistStr: String, outFile: String) extends Serializable {
  val seedLen = seedLenStr.toInt
  val readLen = readLenStr.toInt
  val numSeeds = numSeedsStr.toInt
  val unionDist = unionDistStr.toInt
}

abstract class ParallelSimFinderParams(master: String, seedLenStr: String, readLenStr: String, numSeedsStr: String, unionDistStr: String, outFile: String, 
  minClusterSizeStr: String) 
extends SimFinderParams(seedLenStr, readLenStr, numSeedsStr, unionDistStr, outFile) {
  val minClusterSize = minClusterSizeStr.toInt
  val sparkDest = getSparkDest(master)

  def getSparkDest(master: String): String = {
    if (master.startsWith("local"))
      master
    else
      "1@" + master + ":5050"
  }
}

class StripeParallelSimFinderParams(master: String, seedLenStr: String, readLenStr: String, numSeedsStr: String, unionDistStr: String, outFile: String, 
  numTasksStr: String, minClusterSizeStr: String) 
extends ParallelSimFinderParams(master, seedLenStr, readLenStr, numSeedsStr, unionDistStr, outFile, minClusterSizeStr) {
  val numTasks = numTasksStr.toInt
}

class GridParallelSimFinderParams(master: String, seedLenStr: String, readLenStr: String, numSeedsStr: String, unionDistStr: String, outFile: String, 
  gridDimStr: String, minClusterSizeStr: String) 
extends ParallelSimFinderParams(master, seedLenStr, readLenStr, numSeedsStr, unionDistStr, outFile, minClusterSizeStr) {
  val gridDim = gridDimStr.toInt
}