/*

Author:
    Matei Zaharia
    Developed as part of the SNAP project (http://snap.cs.berkeley.edu/)

*/

package siren

import java.util.Arrays
import scala.math.abs

/** 
 * A simple insert-only hashtable with keys and values as longs. Requires an "empty value"
 * that won't be used by any real entries to be set in order to identify missing entries.
 */
class IntIntMap(emptyValue: Int = 0, estimatedSize: Int = 16, loadFactor: Double = 0.7)
extends Traversable[(Int, Int)] with Serializable
{
  private var elements = 0     // number of elements currently in table
  private var capacity = math.max(10, (estimatedSize / loadFactor).toInt)
  private var data = new Array[Int](2*capacity)
  private var growThreshold = (capacity * loadFactor).toInt
  Arrays.fill(data, emptyValue)
  
  def getData: Array[Int] = data
  
  override def size = elements
  
  def currentCapacity = capacity

  // given a key, return its location in the index
  // performs modified linear probing -- initially increase step size, 
  // then use step size of 1 to guarantee that all cells are visited
  private val STEP_LIMIT = 5
  
  def probe(key: Int): Int = {
    var pos = abs(hash(key)) % capacity
    //println("pos: " + pos)
    var i = 1
    var numProbes = 0
    
    // look for open position near the hash pos if it's taken
    // will terminate upon (1) finding the key, or (2) finding an open position
    while (data(2*pos) != key /* key isn't stored at this pos */ && data(2*pos+1) != emptyValue /* something else is */) {  // this position is occupied, and not by the key
      pos = (pos + i) % capacity
      numProbes += 1
      
      if (numProbes < STEP_LIMIT) 
        i += 1
      else 
        i = 1
        
      //println("i: " + i)
      //println("pos: " + pos)
    }
    
    pos
  }

  def update(key: Int, value: Int): Unit = {
    if (value == emptyValue)
      throw new IllegalArgumentException("update() called with empty value")

    /*
    var pos = abs(hash(key)) % capacity
    //println("in update, pos: " + pos + ", key: " + key)
    var i = 1
    while (data(2*pos) != key && data(2*pos+1) != emptyValue) {
      pos = (pos + i) % capacity
      //println("pos increased to " + pos)
      i += 1
    }
    */
    var pos = probe(key)

    if (data(2*pos) != key || data(2*pos+1) == emptyValue) {
      elements += 1
      if (elements > growThreshold) {
        //println("calling growTable from update")
        growTable()

        // if you call growTable, you also need to recompute the pos, b/c that slot may not be free anymore
        /*
        pos = abs(hash(key)) % capacity
        //println("in update, pos: " + pos + ", key: " + key)
        i = 1
        while (data(2*pos) != key && data(2*pos+1) != emptyValue) {
          pos = (pos + i) % capacity
          //println("pos increased to " + pos)
          i += 1
        }
        */
        pos = probe(key)
      }
    }
    //println("in update, pos: " + pos + ", key: " + key)
    //println("about to update with key " + key + ", pos " + pos)
    data(2*pos) = key
    data(2*pos+1) = value
    
    //println("data updated: " + data.mkString(", "))
  }
    
  def apply(key: Int): Int = {
    /*
    var pos = abs(hash(key)) % capacity
    //println("pos: " + pos)
    //println("key: " + key)
    var i = 1
    while (data(2*pos+1) != emptyValue) {
      //println("pos: " + pos)
      if (data(2*pos) == key)
        return data(2*pos+1)
      pos = (pos + i) % capacity
      i += 1
    }
    return emptyValue
    */
    val pos = probe(key)
    if (data(2*pos) == key)
      data(2*pos+1)
    else
      emptyValue
  }
  
  def getOrUpdate(key: Int, value: Int): Int = {
    if (value == emptyValue)
      throw new IllegalArgumentException("getOrUpdate() called with empty value")

    /*
    var pos = abs(hash(key)) % capacity
    var i = 1
    while (data(2*pos) != key && data(2*pos+1) != emptyValue) {
      pos = (pos + i) % capacity
      i += 1
    }
    */
    var pos = probe(key)
    
    if (data(2*pos) == key) {
      return data(2*pos+1)
    } else {
      data(2*pos) = key
      data(2*pos+1) = value
      elements += 1
      if (elements > growThreshold) {
        //println("calling growTable from getOrUpdate")
        growTable()
      }      
      return emptyValue
    }
  }
  
  // MurmurHash3 from http://sites.google.com/site/murmurhash
  private final def hash(key: Int): Int = {
    var x = key
    x ^= x >>> 16;
    x *= 0x85ebca6b;
    x ^= x >>> 13;
    x *= 0xc2b2ae35;
    x ^= x >>> 16;
    return x;
  }
  
  // Double the capacity and re-hash everything into a new table
  private def growTable() {
    var oldData = data
    var oldCapacity = capacity
    capacity = (capacity * 1.25).toInt
    data = new Array[Int](2 * capacity)
    Arrays.fill(data, emptyValue)
    elements = 0
    for (i <- 0 until oldCapacity)
      if (oldData(2*i + 1) != emptyValue) {
        //println("calling update from growTable with key of " + oldData(2*i))
        update(oldData(2*i), oldData(2*i + 1))
      }
    growThreshold = (capacity * loadFactor).toInt
    //println("returning from growTable")
  }
  
  override def foreach[T](func: ((Int, Int)) => T) {
    for (i <- 0 until capacity)
      if (data(2*i+1) != emptyValue)
        func((data(2*i), data(2*i+1)))
  }
}
