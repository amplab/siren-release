package siren.test

import org.scalatest._
import siren._
import it.unimi.dsi.fastutil.longs.LongList
import it.unimi.dsi.fastutil.longs.LongArrayList

class IntIntMapTest extends FunSuite with PrivateMethodTester {

  //change to 13107 shard: IntIntMap((30896137,1494), (24576156,1496), (30898825,1994), (24606876,496), (30898057,494), (13421736,-2), (30899529,994), (24630428,996))
  //change to 13107 shard: IntIntMap((24606876,496), (24619164,1996), (13421736,-2), (24630428,996), (24576156,1496), (30896137,1494), (30898057,494), (30899529,994))
  test("grow table when capacity is reached") {
    //val getData = PrivateMethod[Array[Int]]('getData)
    val map = new IntIntMap(-1, 0)

    //println("data1: " + map invokePrivate getData())
    //println("data1: " + map.getData.mkString(", "))
    
    // add entries to the map to create almost-full map
    map.getOrUpdate(30896137,1494)
    //println("size: " + map.size + ", capacity: " + map.currentCapacity)
    //println("map: " + map)
    //println("data: " + map.getData.mkString(", "))
    map.getOrUpdate(24576156,1496)
    //println("size: " + map.size + ", capacity: " + map.currentCapacity)
    //println("map: " + map)
    //println("data: " + map.getData.mkString(", "))
    map.getOrUpdate(30898825,1994)
    //println("size: " + map.size + ", capacity: " + map.currentCapacity)
    //println("map: " + map)
    //println("data: " + map.getData.mkString(", "))
    map.getOrUpdate(24606876,496)
    //println("size: " + map.size + ", capacity: " + map.currentCapacity)
    //println("map: " + map)
    //println("data: " + map.getData.mkString(", "))
    map.getOrUpdate(30898057,494)
    //println("size: " + map.size + ", capacity: " + map.currentCapacity)
    //println("map: " + map)
    //println("data: " + map.getData.mkString(", "))
    map.getOrUpdate(13421736,-2)
    //println("size: " + map.size + ", capacity: " + map.currentCapacity)
    //println("map: " + map)
    //println("data: " + map.getData.mkString(", "))
    map.getOrUpdate(30899529,994)
    //println("size: " + map.size + ", capacity: " + map.currentCapacity)
    //println("map: " + map)
    //println("data: " + map.getData.mkString(", "))
    map.getOrUpdate(24630428,996)
    //println("size: " + map.size + ", capacity: " + map.currentCapacity)
    //println("map: " + map)
    //println("data: " + map.getData.mkString(", "))
    /*
    */

    //println("after adding values (should be 1494): " + map(30896137))
    /*
    println("after adding values: " + map(24576156))
    println("after adding values: " + map(30898825))
    println("after adding values: " + map(24606876))
    println("after adding values: " + map(30898057))
    println("after adding values: " + map(13421736))
    println("after adding values: " + map(30899529))
    println("after adding values: " + map(24630428))
    */
    
    // try to add another (should trigger re-sizing)
    //map.getOrUpdate(24619164,1996)
    
    //println("data3: " + map invokePrivate getData())
    //println("data3: " + map.getData.mkString(", "))

    // make sure they're all still there
    /*
    println("in assert: " + map(30896137))
    */
    assert(map.apply(30896137) == 1494)
    /*
    assert(map.apply(24576156) == 1496)
    assert(map.apply(30898825) == 1994)
    assert(map.apply(24606876) == 496)
    assert(map.apply(30898057) == 494)
    assert(map.apply(13421736) == -2)
    assert(map.apply(30899529) == 994)
    assert(map.apply(24630428) == 996)
    assert(map.apply(24619164) == 1996)
    */
  }

  ignore("shouldn't crash on simple example") {
    // init
    val seedLen = 20
    val readLen = 100
    val numTasks = 2
    val numSeeds = 2
    val unionDist = 2
    
    val numBases = GenomeLoader.genome.totalSize
    val rangeLength = (numBases / (numTasks * 2)).toInt
    val startPos = 1500
    val endPos = 2999
    
    // build index for given range
    val builder = new IntHashIndexBuilder(seedLen, rangeLength)
    GenomeLoader.genome.addToIndex(builder, startPos, endPos)
    val index = builder.build()
    println("Finished indexing.")

    // initialize variables
    var validPositions = 0
    val midPoint = (numBases / 2).toInt

    //val valid = Array.fill(numBases.toInt)(false)
    val validL = Array.fill(midPoint)(false)
    val validH = Array.fill(midPoint)(false)
    
    var hits = new LongArrayList
    val unionFindL = new UnionFindL(numBases)
    
    // for each substring, find its hits, and join it via union-find to any similar strings
    //var pos = 0L
    var pos = 415L
    var i = 0
    var j = 0
    val posStr = new Array[Byte](readLen)
    val hitPosStr = new Array[Byte](readLen)
    
    while (pos < numBases) {
      println("pos: " + pos)
      if (pos % 100000 == 0)
        println("Position: " + pos + "(" + math.round(pos * 100.0) / numBases + "%)")
      
      GenomeLoader.genome.getSubstring(pos, pos + readLen, posStr)
      println("posStr: " + (new String(posStr)))
      
      if (!SimFinder.containsN(posStr)) {
        validPositions += 1
        if (pos < midPoint)
          validL(pos.toInt) = true
        else
          validH((pos - midPoint).toInt) = true
      
        // try each seed from the read
        i = 0
        while (i < numSeeds) {
          val seedPos = (i * (readLen - seedLen)) / (numSeeds - 1)
          val seed = DNA.substringToLong(posStr, seedPos, seedPos + seedLen)
          println("seedPos: " + seedPos)
          println("seed: " + (new String(DNA.longToString(seed, seedLen))))
          hits.clear()
          index.get(seed, hits, Int.MaxValue)
          val nHits = hits.size()
        
          j = nHits - 1
          while (j >= 0) {
            val hitPos = hits.getLong(j) - seedPos
            GenomeLoader.genome.getSubstring(hitPos, hitPos + readLen, hitPosStr)
          
            if (hitPos > pos && unionFindL.find(hitPos) != unionFindL.find(pos)) {
              if (!SimFinder.containsN(hitPosStr)) {
                if (HammingDistance.distance(posStr, hitPosStr, unionDist) != -1) {
                  unionFindL.union(pos, hitPos)
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
    
    // placeholder
    //Nil
    unionFindL.findClusters(readLen, SimFinder.containsN)
    
    //(startPos, unionFindL.nonTrivialMembers)
    unionFindL
    
  }

  test("inserting into map:  probe should return location that key hashes to if it's unoccupied") {
    // set up map
    val map = new IntIntMap(0, -1)
    val hash = PrivateMethod[Int]('hash)
    
    // get hashed location
    val key = 31453811
    val hashedLocation = math.abs(map invokePrivate hash(key)) % map.currentCapacity
    
    // map is empty => hashed location is free => should return hashed location
    assert(map.probe(key) == hashedLocation)
  }
  
  test("inserting into map:  probe should return a different location if keys hashed location is occupied") {
    // set up map 
    val emptyValue = -1
    val map = new IntIntMap(emptyValue, 0)
    val hash = PrivateMethod[Int]('hash)
    
    // insert into map
    val key1 = 45629588
    val key1Pos = math.abs(map invokePrivate hash(key1)) % map.currentCapacity
    map.update(key1, 5)

    // attempt to insert another key that hashes to same place into map
    val key2 = 56483367
    val key2Pos = math.abs(map invokePrivate hash(key2)) % map.currentCapacity
    assert(key1Pos == key2Pos)
    //map.update(key2, 6)
    assert(map.probe(key2) != key2Pos)
    assert(map.probe(key2) != emptyValue)
        
    // verify that it gets inserted somewhere else
    /*
    assert(map.getData.indexOf(key1) == 2*key1Pos)  // first key is still in correct place
    assert(map.getData.indexOf(key2) != 2*key2Pos)  // second key is in different place
    assert(map(key1) == 5)  // first value is still there
    assert(map(key2) == 6)  // second value is still there
    */
  }

}