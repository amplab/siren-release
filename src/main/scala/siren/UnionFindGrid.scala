package snap

import it.unimi.dsi.fastutil.longs.LongList
import it.unimi.dsi.fastutil.longs.LongArrayList

class UnionFindGrid(indexRange: (Long, Long), scanRange: (Long, Long)) extends UnionFindAbstract {
  val indexRangeLen = indexRange._2 - indexRange._1 + 1
  val scanRangeLen = scanRange._2 - scanRange._1 + 1
  val totalRangeLen = indexRangeLen + scanRangeLen
  
  // Union Find data structures
  //val parent = ((indexRange._1 to indexRange._2) ++ (scanRange._1 to scanRange._2)).toArray // could be long array list
  val rank = Array.fill(totalRangeLen.toInt)(0)
  val clusterSize = Array.fill(totalRangeLen.toInt)(0)
  var totalClusters = 0
  var nonTrivialClusters = List[Long]()
  var nonTrivialMembers = scala.collection.mutable.Map[Long, LongArrayList]()
  //var firstMember = Array.fill(totalRangeLen.toInt)(0L) // could be long array list
  
  val parent = new LongArrayList(totalRangeLen.toInt)
  val firstMember = new LongArrayList(totalRangeLen.toInt)
  
  var i = 0
  while (i < indexRangeLen) {
    parent.add(i, indexRange._1 + i)
    firstMember.add(i, indexRange._1 + i)
    
    i += 1
  }
  
  var j = 0
  while (j < scanRangeLen) {
    parent.add(i, scanRange._1 + j)
    firstMember.add(i, scanRange._1 + j)
    
    i += 1
    j += 1
  }

  // Methods
  override def size = totalRangeLen

  override def getNonTrivialMembers = nonTrivialMembers
  
  def toIndex(pos: Long): Int = {
    // decide which range it's in
    if (pos >= indexRange._1 && pos <= indexRange._2)
      (pos - indexRange._1).toInt
    else
      (indexRangeLen + (pos - scanRange._1)).toInt
  }

  override def find(v: Long): Long = {
    /*
    var i = 0
    var tmp = 0L

    try {
      i = toIndex(v)
      tmp = parent(i)
    } catch {
      case arrayEx: java.lang.ArrayIndexOutOfBoundsException => {
        println("Caught exception...")
        println("v:  " + v)
        println("indexRange:  (" + indexRange._1 + ", " + indexRange._2 + ")")
        println("scanRange: (" + scanRange._1 + ", " + scanRange._2 + ")")
        println("toIndex(v):  " + i)
        println("parent(toIndex(v)):  " + tmp)
        
        arrayEx.printStackTrace
      }
      case e: Exception => e.printStackTrace
    }
    */
    
    if (v != parent.getLong(toIndex(v)))
      parent.set(toIndex(v), find(parent.getLong(toIndex(v))))
    
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
    //val totalRange = ((indexRange._1 to indexRange._2) ++ (scanRange._1 to scanRange._2)).toArray // don't allocate this!  just use range start & increment
    
    var i = 0
    var pos = 0L
    val posStr = new Array[Byte](readLen)
    
    //while (i < totalRangeLen) {
    while (i < indexRangeLen) {
      pos = indexRange._1 + i
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
      
      i += 1
    }

    var j = 0
    
    while (j < scanRangeLen) {
      pos = scanRange._1 + j
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

      j += 1
    }
    
    nonTrivialClusters = nonTrivialClusters.sortWith((c1, c2) => clusterSize(toIndex(c1)) > clusterSize(toIndex(c2)))
  }
  
  // you pass a function 
  // it'll call the function on every position in the two ranges
  // like the function MA helped me write in the Genome class

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