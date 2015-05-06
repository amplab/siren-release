package siren

import it.unimi.dsi.fastutil.longs.LongList
import it.unimi.dsi.fastutil.longs.LongArrayList
import org.apache.spark.SparkContext
import SparkContext._
import org.apache.spark.SparkConf
//import SparkContext._

object SimFinder {
  def main(args : Array[String]): Unit = {
    // load arguments
    args match {
      case Array(master, sparkLocalDir, seedLenStr, readLenStr, numSeedsStr, unionDistStr, outFile, gridDimStr, minClusterSizeStr) => {
        val startRun = System.currentTimeMillis
        val params = new GridParallelSimFinderParams(master, seedLenStr, readLenStr, numSeedsStr, unionDistStr, outFile, gridDimStr, minClusterSizeStr)
        
        // open output file
        val out = new java.io.File(outFile)
        if (!out.exists)
          out.createNewFile

        val fw = new java.io.FileWriter(out.getName)
        val bw = new java.io.BufferedWriter(fw)
        val numBases = GenomeLoader.genome.totalSize
        val rangeLength = (numBases / (params.gridDim * 2)).toInt  // division by 2 here makes things confusing :(
        val rangeStarts = (0 until (numBases / 2).toInt by rangeLength)

        println("Ranges:")
        rangeStarts.foreach(rangeStart => {
          val startPos = 2 * rangeStart.toLong
          val endPos = math.min(2 * rangeStart.toLong + 2 * rangeLength.toLong, numBases) - 1
          println(startPos + ", " + endPos)
        })

        val partitionStarts = rangeStarts.map(i => List(i).padTo(rangeStarts.length, i).zip(rangeStarts)).flatten
        var ufClusters = new UnionFindL(numBases) // must use "L" here b/c we're dealing with whole genome
        val sparkConf = new SparkConf().setMaster(getSparkDest(master))
                                       .set("spark.driver.maxResultSize", "0")
                                       .setAppName("SimFinder")
                                       .setJars(Seq("target/scala-2.10/siren_2.10-0.0.jar"))
                                       .set("spark.local.dir", sparkLocalDir)
                                       .set("spark.akka.frameSize", "200")
        val sc = new SparkContext(sparkConf)
        val ufAccumulator = sc.accumulator(ufClusters.asInstanceOf[UnionFindAbstract])(UnionFindAP)

        sc.parallelize(partitionStarts, partitionStarts.size).foreach(p => {
          val uf = getPartitionClusters(params, p, rangeLength)
          ufAccumulator += uf
        })
	      println("Finished spark part")

	      println("About to find clusters on ufClusters...")
	      ufClusters = ufAccumulator.value.asInstanceOf[UnionFindL]
        ufClusters.findClusters(params.readLen, containsN)
	      println("Finished finding clusters; about to print stats & to file...")
        
        // Print cluster stats & print to file
        ufClusters.printClusterStats(validPositions(params.readLen))
        ufClusters.writeClustersToFileSnapCompliant(bw, GenomeLoader.genome, params.readLen, params.unionDist, params.minClusterSize)
        bw.close
        println("Run took " + (System.currentTimeMillis - startRun) / 1000.0 + "s")
      }
      case _ => println("Incorrect parameters")
    }
  }
  
  def containsN(str: Array[Byte]): Boolean = {
    var i = 0
    while (i < str.length) {
      if (str(i) == 'N')
        return true
      i += 1
    }
    return false
  }
  
  def validPositions(readLen: Int): Long = {
    var pos = 0L
    var validPositions = 0L
     
    val numBases = GenomeLoader.genome.totalSize
    val posStr = new Array[Byte](readLen)

    while (pos < numBases) {
      GenomeLoader.genome.getSubstring(pos, pos + readLen, posStr)
      
      if (!containsN(posStr))
        validPositions += 1

      pos += 1
    }
    
    validPositions
  }

  def getSparkDest(master: String): String = {
    if (master.startsWith("local"))
      master
    else
      "1@" + master + ":5050"
  }

  def getPartitionClusters(params: ParallelSimFinderParams, p: (Int, Int), rangeLength: Int) = {
    val numBases = GenomeLoader.genome.totalSize
    
    val indexRangeStart = p._1  // range start for row, ie, which positions to index
    val scanRangeStart = p._2   // range start for column, ie, which positions to scan over for clustering
    
    val indexStartPos = 2 * indexRangeStart.toLong
    val indexEndPos = math.min(indexStartPos + 2 * rangeLength.toLong, numBases) - 1

    val scanStartPos = 2 * scanRangeStart.toLong
    val scanEndPos = math.min(scanStartPos + 2 * rangeLength.toLong, numBases) - 1
    
    println("indexing " + indexStartPos + " to " + indexEndPos + "; scanning " + scanStartPos + " to " + scanEndPos)

    // build index for given range
    val builder = new IntHashIndexBuilder(params.seedLen, 2 * rangeLength)
    GenomeLoader.genome.addToIndex(builder, indexStartPos, indexEndPos)
    val index = builder.build()
    
    // initialize variables
    var validPositions = 0
    
    // TODO:  keep track of valid positions -- a bit difficult
    // need to do similar thing to what i did with grid, grid diagonal versions of union find (different behavior if range is same or not)
    // TODO:  I think I can skip it, if I just use containsN
    
    var hits = new LongArrayList
    
    val uf = 
    if (indexStartPos == scanStartPos) new UnionFindGridDiagonal((indexStartPos, indexEndPos))
    else new UnionFindGrid((indexStartPos, indexEndPos), (scanStartPos, scanEndPos))
    
    // for each substring, find its hits, and join it via union-find to any similar strings
    var pos = scanStartPos
    var i = 0
    var j = 0
    val posStr = new Array[Byte](params.readLen)
    val hitPosStr = new Array[Byte](params.readLen)
    
    while (pos < scanEndPos) {
      if (pos % 100000 == 0)
        println("Position: " + pos + "(" + math.round((pos - scanStartPos) * 100.0) / (2 * rangeLength) + "%)")
      
      GenomeLoader.genome.getSubstring(pos, pos + params.readLen, posStr)
      
      if (!containsN(posStr)) {
        //validPositions += 1
        // TODO:  update valid positions array
        // TODO:  figure out if I can skip this entirely
        
        // try each seed from the read
        i = 0
        while (i < params.numSeeds) {
          val seedPos = (i * (params.readLen - params.seedLen)) / (params.numSeeds - 1)
          val seed = DNA.substringToLong(posStr, seedPos, seedPos + params.seedLen)
          hits.clear()
          index.get(seed, hits, Int.MaxValue)
          val nHits = hits.size()
          
          j = nHits - 1
          while (j >= 0) {
            val hitPos = hits.getLong(j) - seedPos
            GenomeLoader.genome.getSubstring(hitPos, hitPos + params.readLen, hitPosStr)

      	    // hitPos could be out of range if a seed that occurs early in the range occurs late in the read (makes read extend into prior range)
      	    // so, make sure hitPos is within range
            if (hitPos >= indexStartPos && hitPos > pos && uf.find(hitPos) != uf.find(pos)) {
              if (!containsN(hitPosStr)) {
                if (HammingDistance.distance(posStr, hitPosStr, params.unionDist) != -1) {
                  uf.union(pos, hitPos)
                }
              }
            } else if (hitPos <= pos) {
              j = -1
            }
            
            j -= 1
          }
          
          i += 1
        }
      }
      
      pos += 1
    }
    
    // return clusters
    uf.findClusters(params.readLen, containsN)
    uf
  }
}
