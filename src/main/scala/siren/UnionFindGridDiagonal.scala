/*

Author:
    Kristal Curtis

*/

package siren

import it.unimi.dsi.fastutil.longs.LongList
import it.unimi.dsi.fastutil.longs.LongArrayList

// assumes range is inclusive on both endpoints
class UnionFindGridDiagonal(range: (Long, Long)) extends UnionFindAbstract {
  val rangeLen = range._2 - range._1 + 1
  
  // Union Find data structures
  //val parent = (range._1 to range._2).toArray // turn this into LongArrayList
  val rank = Array.fill(rangeLen.toInt)(0)  // can be int
  val clusterSize = Array.fill(rangeLen.toInt)(0) // can be int because a cluster size will in all likelihood be < 2B 
  var totalClusters = 0
  var nonTrivialClusters = List[Long]() // needs to be long b/c you're storing cluster ids, which are positions of a member in the cluster
  var nonTrivialMembers = scala.collection.mutable.Map[Long, LongArrayList]()
  //var firstMember = Array.fill(rangeLen.toInt)(0L)  // needs to be long b/c this is storing positions // turn this into LongArrayList

  val parent = new LongArrayList(rangeLen.toInt)
  val firstMember = new LongArrayList(rangeLen.toInt)
  
  var i = 0
  while (i < rangeLen) {
    parent.add(i, range._1 + i)
    firstMember.add(i, range._1 + i)
    
    i += 1
  }

  // Methods
  override def size = rangeLen

  override def getNonTrivialMembers = nonTrivialMembers

  def toIndex(pos: Long): Int = {
    (pos - range._1).toInt  // convert pos in range to index in array
  }

  override def find(v: Long): Long = {
    if (v != parent.getLong(toIndex(v)))
      // parent(v) = find(parent(v))
      parent.set(toIndex(v), find(parent.getLong(toIndex(v))))  // this line is causing stack overflow
    
    return parent.getLong(toIndex(v))
  }
  
  override def union(v: Long, w: Long) = {
    val x = find(v)
    val y = find(w)
    
    if (rank(toIndex(x)) > rank(toIndex(y))) {
      parent.set(toIndex(y), x)
    } else {
      parent.set(toIndex(x), y)
      if (rank(toIndex(y)) == rank(toIndex(x)))
        rank(toIndex(y)) += 1
    }
  }

  // assumes nontrivial cluster size is 2
  override def findClusters(readLen: Int, containsN: (Array[Byte] => Boolean)) = {
    var pos = range._1
    val posStr = new Array[Byte](readLen)

    while (pos <= range._2 - readLen) {
      GenomeLoader.genome.getSubstring(pos, pos + readLen, posStr)

      //if (isValid(pos, pos + readLen)) {
      if (!containsN(posStr)) {
        val p = find(pos)
        clusterSize(toIndex(p)) += 1
        
        if (clusterSize(toIndex(p)) == 1) {
          totalClusters += 1
          firstMember.set(toIndex(p), pos)  // Remember in case cluster becomes non-trivial
        } else if (clusterSize(toIndex(p)) == 2) {
          // Just found a new non-trivial cluster (with more than one element)
          nonTrivialClusters = p :: nonTrivialClusters
          val l = new LongArrayList
          l.add(pos)
          l.add(firstMember.getLong(toIndex(p)))
          nonTrivialMembers += ((p, l))
        } else if (clusterSize(toIndex(p)) > 2) {
          val members = nonTrivialMembers.get(p).get
          members.add(pos)
        }
      }
      
      pos += 1
    }
    
    nonTrivialClusters = nonTrivialClusters.sortWith((c1, c2) => clusterSize(toIndex(c1)) > clusterSize(toIndex(c2)))
  }

  override def printClusterStats(validPositions: Long) = {
    println("Valid positions: " + validPositions)
    println("Total clusters: " + totalClusters)
    println("Non-trivial clusters: " + nonTrivialClusters.size)

    val nonTrivialPositions = validPositions - (totalClusters - nonTrivialClusters.size)
    
    println("Positions in non-trivial clusters: " + nonTrivialPositions)
    println("Mean size of non-trivial clusters: " + nonTrivialPositions / nonTrivialClusters.size.toDouble)
  }
  
  override def getStats(validPositions: Long): String = {
    val nonTrivialPositions = validPositions - (totalClusters - nonTrivialClusters.size)

    "Valid positions: " + validPositions + "\n" +
    "Total clusters: " + totalClusters + "\n" +
    "Non-trivial clusters: " + nonTrivialClusters.size + "\n" +
    "Positions in non-trivial clusters: " + nonTrivialPositions + "\n" +
    "Mean size of non-trivial clusters: " + (nonTrivialPositions / nonTrivialClusters.size.toDouble) + "\n"
  }
}