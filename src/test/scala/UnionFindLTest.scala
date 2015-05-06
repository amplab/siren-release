package siren.test

import org.scalatest._
import siren._
import it.unimi.dsi.fastutil.longs.LongArrayList

class UnionFindLTest extends FunSuite with BeforeAndAfter {
  val unionFindL = new UnionFindL(10L)
  
  test("find is correct, initially") {
    assert(unionFindL.find(5) == 5)
  }
  
  test("union is correct") {
    unionFindL.union(2, 3)
    assert(unionFindL.parentL(2) == 3)  // hacky -- should check whether the function can correctly look in either parentL or parentH
    assert(unionFindL.parentL(3) == 3)
  }
  
  test("find is correct, after union") {
    unionFindL.union(2, 3)
    assert(unionFindL.find(2) == 3)
  }
  
  // ignored tests are out of date with current implementation of UnionFindL
  ignore("find clusters is correct") {
    unionFindL.union(2,3)
    unionFindL.union(7,8)

    unionFindL.findClusters(0, SimFinder.containsN)

    println(unionFindL.totalClusters)
    println(unionFindL.nonTrivialClusters.length)

    assert(unionFindL.totalClusters == 8)
    assert(unionFindL.nonTrivialClusters.length == 2)
  }
  
  ignore("sort is correct") {
    val uf = new UnionFindL(GenomeLoader.genome.totalSize)
    
    // initialize clusters
    uf.nonTrivialClusters += 100
    uf.nonTrivialClusters += 200
    
    // add members for cluster 1
    val l1 = new LongArrayList
    l1.add(100)
    l1.add(105)
    l1.add(110)
    uf.nonTrivialMembers += ((100, l1))
    
    val l2 = new LongArrayList
    l2.add(200)
    l2.add(205)
    l2.add(210)
    uf.nonTrivialMembers += ((200, l2))

    uf.clusterSizeL(100) = 3
    uf.clusterSizeL(200) = 3
    
    uf.sortNonTrivialClusters
    
    assert(uf.nonTrivialClusters(0) == 100)
  }

  test("recovering long from overflowed int should return original long") {
    var origLongs = List(2500, 2e9.toInt, 2.5e9.toLong, 3e9.toLong, 2.7e9.toLong, 2147483649L, 2499999998L)
    origLongs.foreach(origLong => {
      var intVal = origLong.toInt
      assert(unionFindL.recoverLong(intVal) == origLong)
    })
  }
}