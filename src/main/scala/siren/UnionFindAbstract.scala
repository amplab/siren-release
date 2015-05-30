/*

Author:
    Kristal Curtis

*/

package siren

import it.unimi.dsi.fastutil.longs.LongList
import it.unimi.dsi.fastutil.longs.LongArrayList
import org.apache.spark._

abstract class UnionFindAbstract extends Serializable {
  def size: Long

  def find(v: Long): Long
  
  def union(v: Long, w: Long)
  
  def findClusters(readLen: Int, containsN: (Array[Byte] => Boolean))
  
  def printClusterStats(validPositions: Long)

  def getNonTrivialMembers: scala.collection.mutable.Map[Long, LongArrayList]

  def getStats(validPositions: Long): String
}

class UnionFindEmpty extends UnionFindAbstract {
  override def size = 0L
  
  override def find(v: Long): Long = -1L
  
  override def union(v: Long, w: Long) { }
  
  override def findClusters(readLen: Int, containsN: (Array[Byte] => Boolean)) { }
  
  override def printClusterStats(validPositions: Long) { }

  override def getNonTrivialMembers: scala.collection.mutable.Map[Long, LongArrayList] = null

  override def getStats(validPositions: Long): String = ""
}

// could put inside object UnionFindAbstract
// then import UnionFindAbstract._
//object Wrapper {
  /*implicit*/ object UnionFindAP extends AccumulatorParam[UnionFindAbstract] {
    def zero(uf: UnionFindAbstract) = new UnionFindEmpty //new UnionFindL(uf.size) // make this union find empty; just call size

    // if first is empty, just return 2nd
    // if first is nonempty, do this merge
    def addInPlace(uf1: UnionFindAbstract /* aggregate, ie, UnionFindL */, uf2: UnionFindAbstract /* incremental, ie, UnionFindGrid(Diagonal) */) = {
      /*
      if (uf1.size == 0L) {
        //uf2
	val tmp1 = new UnionFindL(GenomeLoader.genome.totalSize)	// wrong value for totalSize?

        val clusters = uf2.getNonTrivialMembers

        clusters.keySet.foreach(k => {
          val members = clusters.get(k).get
          val firstMember = members.getLong(0)  // convert to pos; requires knowing whether this was grid or grid diagonal
          var m = 1
          while (m < members.size) {
            val currentMember = members.getLong(m)  // convert to pos; requires knowing whether this was grid or grid diagonal
            if (tmp1.find(firstMember) != tmp1.find(currentMember))
              tmp1.union(firstMember, currentMember)

            m += 1
          }
        })

        tmp1
      }
      */
      if (uf1.size == 0L) {
        //println("in case 1 of addInPlace; uf1 type: " + uf1.getClass + ", uf2 type: " + uf2.getClass)
        uf2
      } else {
        //println("in case 2 of addInPlace; uf1 type: " +	uf1.getClass + ", uf2 type: " +	uf2.getClass)
        val clusters = uf2.getNonTrivialMembers

        clusters.keySet.foreach(k => {
          val members = clusters.get(k).get
          val firstMember = members.getLong(0)  // convert to pos; requires knowing whether this was grid or grid diagonal
          var m = 1
          while (m < members.size) {
            val currentMember = members.getLong(m)  // convert to pos; requires knowing whether this was grid or grid diagonal
            if (uf1.find(firstMember) != uf1.find(currentMember))
              uf1.union(firstMember, currentMember)

            m += 1
          }
        })

        uf1
      }
    }
  }
//}


