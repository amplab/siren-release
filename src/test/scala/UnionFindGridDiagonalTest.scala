package siren.test

import org.scalatest._
import siren._
import it.unimi.dsi.fastutil.longs.LongArrayList

class UnionFindGridDiagonalTest extends FunSuite {
  test("convert pos in genome to index in arrays") {
    val rangeEndpoints = (1000L, 1999L)
    val uf = new UnionFindGridDiagonal(rangeEndpoints)
    assert(uf.toIndex(1000L) == 0)
    assert(uf.toIndex(1999L) == 999)
    assert(uf.toIndex(1200L) == 200)
  }
  
  ignore("on snippet, gives same results as UnionFind and UnionFindL") {
    val seedLen = 20
    val readLen = 100
    val numSeeds = 5
    val unionDist = 3
    val fastaFile = "/Users/kcurtis/workspace/snapFiles/snippet.fa"
    
    // load genome
    val genome = FASTA.read(fastaFile)
    val numBases = genome.totalSize

    // build index
    val builder = new IntHashIndexBuilder(seedLen, numBases)
    genome.addToIndex(builder)
    val index = builder.build()

    // initialize variables
    var validPositions = 0
    val valid = Array.fill(numBases.toInt)(false)
    var hits = new LongArrayList
    val unionFind = new UnionFindGridDiagonal((0L, (numBases - 1).toLong))

    // for each substring, find its hits, and join it via union-find to any similar strings
    var pos = 0L
    var i = 0
    var j = 0
    val posStr = new Array[Byte](readLen)
    val hitPosStr = new Array[Byte](readLen)
    
    while (pos < numBases) {
      if (pos % 100000 == 0)
        println("Position: " + pos + "(" + math.round(pos * 100.0) / numBases + "%)")
        
      genome.getSubstring(pos, pos + readLen, posStr)
      
      if (!SimFinder.containsN(posStr)) {
        validPositions += 1
        valid(pos.toInt) = true
        
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
            genome.getSubstring(hitPos, hitPos + readLen, hitPosStr)
            
            if (hitPos > pos && unionFind.find(hitPos.toInt) != unionFind.find(pos.toInt)) {
              if (!SimFinder.containsN(hitPosStr)) {
                if (HammingDistance.distance(posStr, hitPosStr, unionDist) != -1) {
                  unionFind.union(pos.toInt, hitPos.toInt)
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

    // finalize clusters
    unionFind.findClusters(readLen, SimFinder.containsN)

    assert(validPositions == 2901)
    assert(unionFind.totalClusters == 500)
    assert(unionFind.nonTrivialClusters.size == 500)
    val nonTrivialPositions = validPositions - (unionFind.totalClusters - unionFind.nonTrivialClusters.size)
    assert(nonTrivialPositions == 2901)
    assert(BigDecimal(nonTrivialPositions / unionFind.nonTrivialClusters.size.toDouble).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble == 5.802)
  }
}