/*

Author:
    Kristal Curtis

*/

package siren

import it.unimi.dsi.fastutil.longs.LongList
import it.unimi.dsi.fastutil.longs.LongArrayList
import scala.collection.mutable.ArrayBuffer

class UnionFindL(numBases: Long) extends UnionFindAbstract {
  val midPoint = (numBases / 2).toInt
  
  val parentL = Array.fill(midPoint)(0) // 0 to midPoint - 1
  val parentH = Array.fill((numBases - midPoint).toInt)(0) // midPoint to numBases - 1

  var i = 0
  while (i < numBases) {
    if (i < midPoint) 
      parentL(i) = i
    else
      parentH((i - midPoint)) = i
    
    i += 1
  }
  
  val rankL = Array.fill(midPoint)(0)
  val rankH = Array.fill((numBases - midPoint).toInt)(0)

  val clusterSizeL = Array.fill(midPoint)(0)
  val clusterSizeH = Array.fill((numBases - midPoint).toInt)(0)

  var totalClusters = 0L
  // note that I'm assuming I'll have < 2 billion clusters
  var nonTrivialClusters = ArrayBuffer[Long]()
  var nonTrivialMembers = scala.collection.mutable.Map[Long, LongArrayList]()
  
  val firstMemberL = Array.fill(midPoint)(0)
  val firstMemberH = Array.fill((numBases - midPoint).toInt)(0)

  override def size = numBases

  override def find(v: Long): Long = {
    val vParent = 
    if (v < midPoint)
      recoverLong(parentL(v.toInt))
    else
      recoverLong(parentH((v - midPoint).toInt))
    
    if (v != vParent) {
      //parent(v) = find(parent(v))
      if (v < midPoint)
        parentL(v.toInt) = find(vParent).toInt
      else
        parentH((v - midPoint).toInt) = find(vParent).toInt
    }
    //return parent(v)
    if (v < midPoint)
      recoverLong(parentL(v.toInt))
    else
      recoverLong(parentH((v - midPoint).toInt))
  }
  
  override def union(v: Long, w: Long) = {
    val x = find(v)
    val y = find(w)

    val rankX = 
    if (x < midPoint)
      rankL(x.toInt)
    else
      rankH((x - midPoint).toInt)
      
    val rankY = 
    if (y < midPoint)
      rankL(y.toInt)
    else
      rankH((y - midPoint).toInt)

    if (rankX > rankY) {
      //parent(y) = x
      if (y < midPoint)
        parentL(y.toInt) = x.toInt
      else
        parentH((y - midPoint).toInt) = x.toInt
    } else {
      //parent(x) = y
      if (x < midPoint)
        parentL(x.toInt) = y.toInt
      else
        parentH((x - midPoint).toInt) = y.toInt
      
      /*
      if (rank(y) == rank(x))
        rank(y) += 1
      */
      if (rankY == rankX)
        if (y < midPoint)
          rankL(y.toInt) += 1
        else
          rankH((y - midPoint).toInt) += 1
    }
  }

  //def findClusters(readLen: Int, containsN: (Array[Byte] => Boolean))
  override def findClusters(readLen: Int, containsN: (Array[Byte] => Boolean)) = {
    var pos = 0L
    val posStr = new Array[Byte](readLen)

    while (pos < numBases - readLen) {
      GenomeLoader.genome.getSubstring(pos, pos + readLen, posStr)

      //if (isValid(pos, pos + readLen)) {
      if (!containsN(posStr)) {
        val p = find(pos)
        if (p < midPoint)
          clusterSizeL(p.toInt) += 1
        else
          clusterSizeH((p - midPoint).toInt) += 1
        
        //if (clusterSize(p) == 1) {
        val clusterSizeP = 
        if (p < midPoint)
          clusterSizeL(p.toInt)
        else
          clusterSizeH((p - midPoint).toInt)

        if (clusterSizeP == 1) {
          totalClusters += 1
          //firstMember(p) = pos  // Remember in case cluster becomes non-trivial
          if (p < midPoint)
            firstMemberL(p.toInt) = pos.toInt
          else
            firstMemberH((p - midPoint).toInt) = pos.toInt
        //} else if (clusterSize(p) == 2) {
        } else if (clusterSizeP == 2) {
          // Just found a new non-trivial cluster (with more than one element)
          nonTrivialClusters += p
          val l = new LongArrayList
          //l.add(pos)
          val firstMemberP =
          if (p < midPoint)
            recoverLong(firstMemberL(p.toInt))
          else
            recoverLong(firstMemberH((p - midPoint).toInt))
          l.add(firstMemberP)
	        l.add(pos)
          nonTrivialMembers += ((p, l))
        //} else if (clusterSize(p) > 2) {
        } else if (clusterSizeP > 2) {
          val members = nonTrivialMembers.get(p).get
          members.add(pos)
        }
      }
      
      pos += 1
    }
    
    /*
    nonTrivialClusters = nonTrivialClusters.sortWith((c1, c2) => {
      val clusterSize1 = 
      if (c1 < midPoint)
        clusterSizeL(c1.toInt)
      else
        clusterSizeH((c1 - midPoint).toInt)
        
      val clusterSize2 =
      if (c2 < midPoint)
        clusterSizeL(c2.toInt)
      else
        clusterSizeH((c2 - midPoint).toInt)

      clusterSize1 > clusterSize2
    })
    */
    sortNonTrivialClusters
  }

  def sortNonTrivialClusters = {
    nonTrivialClusters = nonTrivialClusters.sortWith((c1, c2) => {
      val clusterSize1 = clusterSizeById(c1)
      val clusterSize2 = clusterSizeById(c2)

      if (clusterSize1 != clusterSize2) {
        clusterSize1 > clusterSize2
      } else {
        // sort by first member
        val firstMember1 = nonTrivialMembers(c1).getLong(0)
        val firstMember2 = nonTrivialMembers(c2).getLong(0)
        firstMember1 < firstMember2
      }
    })
  }

  override def getNonTrivialMembers: scala.collection.mutable.Map[Long, LongArrayList] = nonTrivialMembers

  def clusterSizeById(id: Long): Long = {
    if (id < midPoint)
      clusterSizeL(id.toInt)
    else
      clusterSizeH((id - midPoint).toInt)
  }

  override def printClusterStats(validPositions: Long) = {
    println("Valid positions: " + validPositions)
    println("Total clusters: " + totalClusters)
    println("Non-trivial clusters: " + nonTrivialClusters.size)

    val nonTrivialPositions = validPositions - (totalClusters - nonTrivialClusters.size)
    
    println("Positions in non-trivial clusters: " + nonTrivialPositions)
    println("Mean size of non-trivial clusters: " + nonTrivialPositions / nonTrivialClusters.size.toDouble)
  }

  def writeClustersToFile(bw: java.io.BufferedWriter, genome: Genome, readLen: Int, minClusterSize: Int = 5) = {
    var numPrinted = 0
    var i = 0
    val clusterRep = new Array[Byte](readLen)
    val buf1 = new Array[Byte](readLen)
    val buf2 = new Array[Byte](readLen)
    
    while (i < nonTrivialClusters.size) {
      val id = nonTrivialClusters(i)
      //if (clusterSize(id) > minClusterSize) {
      val clusterSizeId = 
      if (id < midPoint)
        clusterSizeL(id.toInt)
      else
        clusterSizeH((id - midPoint).toInt)
      
      if (clusterSizeId >= minClusterSize) {
        genome.getSubstring(id, id + readLen, clusterRep)  // get first member of cluster; should eventually be consensus string
        val members = nonTrivialMembers(id)
        // Compute maximum & average Hamming distance among pairs in cluster
        var maxDist = 0
        var sumDists = 0
        var j = 0
        while (j < members.size) {
          genome.getSubstring(members.getLong(j), members.getLong(j) + readLen, buf1)
          var k = j + 1
          while (k < members.size) {
            genome.getSubstring(members.getLong(k), members.getLong(k) + readLen, buf2)
            val dist = HammingDistance.distance(buf1, buf2, readLen)
            maxDist = math.max(dist, maxDist)
            sumDists += dist
            
            k += 1
          }
          
          j += 1
        }
        
        val avgDist = sumDists.toDouble / (members.size * (members.size - 1) / 2)

        // Print info about the cluster
        bw.write(List(members.size, avgDist, maxDist, readLen, new String(clusterRep)).mkString(" "))
        j = 0
        while (j < members.size) {
          bw.write(" " + members.getLong(j))
          
          j += 1
        }
        bw.newLine
        
        numPrinted += 1
      } else {
        i = nonTrivialClusters.size
      }
      
      i += 1
    }

    println("Printed out " + numPrinted + " clusters of size > " + minClusterSize)
  }

  // output format expected by SNAP
  // Line 1:  <genomeLength> <numClusters> <stringLength> <mergeDist>
  // For each cluster:  <numMembers> <member1> <member2> <...>
  def writeClustersToFileSnapCompliant(bw: java.io.BufferedWriter, genome: Genome, readLen: Int, mergeDist: Int, minClusterSize: Int = 5) = {
    var numPrinted = 0
    var i = 0
    
    // print header line
    // numClusters should be # clusters that I'm printing out (ie, with size above minClusterSize)
    bw.write(List(genome.totalSize, nonTrivialClusters.size, readLen, mergeDist).mkString(" "))
    bw.newLine

    while (i < nonTrivialClusters.size) {
      val id = nonTrivialClusters(i)
      val clusterSizeId = 
      if (id < midPoint)
        clusterSizeL(id.toInt)
      else
        clusterSizeH((id - midPoint).toInt)
      
      if (clusterSizeId >= minClusterSize) {
        val members = nonTrivialMembers(id)

        // Print info about the cluster
        //bw.write(List(members.size, avgDist, maxDist, readLen, new String(clusterRep)).mkString(" "))
        bw.write(members.size.toString)
        var j = 0
        while (j < members.size) {
          bw.write(" " + members.getLong(j))
          
          j += 1
        }
        bw.newLine
        
        numPrinted += 1
      } else {
        i = nonTrivialClusters.size
      }
      
      i += 1
    }

    println("Printed out " + numPrinted + " clusters of size > " + minClusterSize)
  }

  override def getStats(validPositions: Long): String = {
    val nonTrivialPositions = validPositions - (totalClusters - nonTrivialClusters.size)

    "Valid positions: " + validPositions + "\n" +
    "Total clusters: " + totalClusters + "\n" +
    "Non-trivial clusters: " + nonTrivialClusters.size + "\n" +
    "Positions in non-trivial clusters: " + nonTrivialPositions + "\n" +
    "Mean size of non-trivial clusters: " + (nonTrivialPositions / nonTrivialClusters.size.toDouble) + "\n"
  }

  def recoverLong(intVal: Int): Long = {
    intVal - Int.MinValue + 1 + Int.MaxValue.toLong
  }
} 