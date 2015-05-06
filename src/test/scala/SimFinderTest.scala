package siren.test

import org.scalatest._
import siren._
import it.unimi.dsi.fastutil.longs.LongList
import it.unimi.dsi.fastutil.longs.LongArrayList

class SimFinderTest extends FunSuite {
  test("get spark dest when answer is local") {
    assert(SimFinder.getSparkDest("local") == "local")
  }
  
  test("get spark dest when answer is local[n]") {
    assert(SimFinder.getSparkDest("local[8]") == "local[8]")
  }
  
  test("get spark dest when passed an IP") {
    assert(SimFinder.getSparkDest("ec2-23-22-5-212.compute-1.amazonaws.com") == "1@ec2-23-22-5-212.compute-1.amazonaws.com:5050")
  }
  
  ignore("shouldn't crash on medium-sized example (chr1-3)") {
    // init
    val seedLen = 20
    val readLen = 100
    val numSeeds = 5
    val unionDist = 3

    // reproduce the failure on the partition that makes the job crash due to array out of bounds exception
    val indexStartPos = 552377920L
    val indexEndPos = 586901539L
    
    val scanStartPos = 0L
    val scanEndPos = 34523619L
    
    println("indexing " + indexStartPos + " to " + indexEndPos + "; scanning " + scanStartPos + " to " + scanEndPos)

    // build index for given range
    val indexRangeLen = indexEndPos - indexStartPos + 1
    val builder = new IntHashIndexBuilder(seedLen, indexRangeLen)
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
    val posStr = new Array[Byte](readLen)
    val hitPosStr = new Array[Byte](readLen)
    
    val scanRangeLen = scanEndPos - scanStartPos + 1
    while (pos < scanEndPos) {
      if (pos % 100000 == 0)
        println("Position: " + pos + "(" + math.round((pos - scanStartPos) * 100.0) / (2 * scanRangeLen) + "%)")
      
      GenomeLoader.genome.getSubstring(pos, pos + readLen, posStr)
      
      if (!SimFinder.containsN(posStr)) {
        validPositions += 1
        // TODO:  update valid positions array
        // TODO:  figure out if I can skip this entirely
        
        // try each seed from the read
        i = 0
        while (i < numSeeds) {
          val seedPos = (i * (readLen - seedLen)) / (numSeeds - 1)
          val seed = DNA.substringToLong(posStr, seedPos, seedPos + seedLen)
          hits.clear()
          index.get(seed, hits, Int.MaxValue)
          val nHits = hits.size()
          
          j = nHits - 1
          while (j >= 0) {
            val hitPos = hits.getLong(j) - seedPos
            GenomeLoader.genome.getSubstring(hitPos, hitPos + readLen, hitPosStr)
            
      	    /*
      	    if (hitPos == 552377821 || pos == 552377821) {
      	      println("pos:  " + pos)
      	      println("hitPos:  " + hitPos)
      	      println("seedPos:  " + seedPos)
      	      println("seed:  " + seed + ", ie, " + DNA.longToString(seed, seedLen))
      	      println("hits:  " + hits.toArray.mkString(", "))
      	    }
      	    */

      	    // hitPos could be out of range if a seed that occurs early in the range occurs late in the read (makes read extend into prior range)
      	    // so, make sure hitPos is within range
            if (hitPos >= indexStartPos && hitPos > pos && uf.find(hitPos) != uf.find(pos)) {
              if (!SimFinder.containsN(hitPosStr)) {
                if (HammingDistance.distance(posStr, hitPosStr, unionDist) != -1) {
                  uf.union(pos, hitPos)
                }
              }
            } else if (hitPos <= pos) {  // is this still true?
              j = -1
            }
            
            j -= 1
          }
          
          i += 1
        }
      }
      
      pos += 1
    }
  }

  /*
  test("get unique partitions in grid") {
    val numBases = 1000
    val gridDim = 4
    println(SimFinder.getUniquePartitions(1000, 4))
    assert(SimFinder.getUniquePartitions(1000, 4) == List((0, 0), (0, 250), (0, 500), (0, 750),
                                                                  (250, 250), (250, 500), (250, 750),
                                                                            (500, 500), (500, 750),
                                                                                      (750, 750)))
  }
  */

  /*
  test("should return same clusters when run on symmetric partitions") {
    val params = new GridParallelSimFinderParams("local", "20", "100", "5", "3", "out.txt", "4", "2")

    // assumes snippet is being loaded by GenomeLoader
    val numBases = GenomeLoader.genome.totalSize  // 3000
    val rangeLength = 750

    val uf1 = SimFinder.getPartitionClusters(params, (0, 750), rangeLength)
    val uf2 = SimFinder.getPartitionClusters(params, (750, 0), rangeLength)

    println("uf1:")
    println(uf1.getStats(2901))
    println
    println("uf2:")
    println(uf2.getStats(2901))
    println
    
    assert(uf1.getStats(2901) == uf2.getStats(2901))
  }
  */
}