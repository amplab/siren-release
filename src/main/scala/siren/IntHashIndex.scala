package siren

import it.unimi.dsi.fastutil.ints.IntArrayList
import it.unimi.dsi.fastutil.longs.LongList
import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap
import scala.math.min
import scala.util.control.Breaks.{break, breakable}

class IntHashIndex(seedLen: Int, estimatedSize: Long) extends Index(seedLen) with Serializable {
  private val SHARD_BITS = 14  // Allows seeds of up to 21 bases
  private val SHARDS = 1 << SHARD_BITS
  private val SHARD_MASK = SHARDS - 1
  private val NO_HIT = -1
  private val MULTI_HIT = -2
  
  // Contain a position for single-hit seeds, or MULTI_HIT for multiple
  private val maps = Array.tabulate(SHARDS)(_ => new IntIntMap(NO_HIT, (estimatedSize * 0.95 / SHARDS).toInt, loadFactor = 0.8))
  
  // Contains only lists of positions for multi-hit seeds
  private val overflow = new Long2ObjectOpenHashMap[IntArrayList]
  
  def getMultiHitMap: Long2ObjectOpenHashMap[IntArrayList] = overflow
  
  def add(seed: Long, pos: Long): Unit = {
    val shard = (seed & SHARD_MASK).toInt
    val key = (seed >> SHARD_BITS).toInt
    val map = maps(shard)
    val old = map.getOrUpdate(key, pos.toInt)
    if (old != NO_HIT) {
      if (old == MULTI_HIT) {
        val list = overflow.get(seed)
        //if (list.size() < 100)  // controls hit list length; commented out for now b/c I want to investigate popular seeds
          list.add(pos.toInt)
      } else {
        val list = new IntArrayList(2)
        list.add(old)
        list.add(pos.toInt)
        overflow.put(seed, list)
        map(key) = MULTI_HIT
      }
    }
  }

  override def get(seed: Long, dest: LongList, max: Int): Boolean = {
    val shard = (seed & SHARD_MASK).toInt
    val key = (seed >> SHARD_BITS).toInt
    val map = maps(shard)
    val p = map(key)
    if (p == NO_HIT || max == 0) {
      return true
    } else if (p == MULTI_HIT) {
      val list = overflow.get(seed)
      if (list.size() > max) 
        return false
      var i = 0
      while (i < list.size()) {
        dest.add(list.get(i).toLong & 0xffffffffL)
        i += 1
      }
      return true
    } else {
      dest.add(p.toLong & 0xffffffffL)
    }
  }

  def printStats() {
    import scala.collection.JavaConversions._
    println("Total entries: " + maps.map(_.size.toLong).sum)
    println("Overflow entries: " + overflow.size())
    val values = overflow.values
    println("Overflow of 2: " + values.count(_.size() == 2))
    println("Overflow of 3: " + values.count(_.size() == 3))
    println("Overflow of 4-10: " + values.count(x => x.size() >= 4 && x.size() <= 10))
    //println("Overflow of 11-100: " + values.count(x => x.size() >= 11 && x.size() <= 100))
    println("Overflow of 11-20: " + values.count(x => x.size() >= 11 && x.size() <= 20))
    println("Overflow of 21-30: " + values.count(x => x.size() >= 21 && x.size() <= 30))
    println("Overflow of 31-40: " + values.count(x => x.size() >= 31 && x.size() <= 40))
    println("Overflow of 41-50: " + values.count(x => x.size() >= 41 && x.size() <= 50))
    println("Overflow of 51-60: " + values.count(x => x.size() >= 51 && x.size() <= 60))
    println("Overflow of 61-70: " + values.count(x => x.size() >= 61 && x.size() <= 70))
    println("Overflow of 71-80: " + values.count(x => x.size() >= 71 && x.size() <= 80))
    println("Overflow of 81-90: " + values.count(x => x.size() >= 81 && x.size() <= 90))
    println("Overflow of 91-100: " + values.count(x => x.size() >= 91 && x.size() <= 100))
    
    (91 to 100).foreach(i => {
      println("Overflow of " + i + ": " + values.count(_.size() == i))
    })
    
    println("Overflow of 101-1000: " + values.count(x => x.size() >= 101 && x.size() <= 1000))
    println("Overflow of > 1000: " + values.count(x => x.size() >= 1001))
  }
  
  def getPopularSeeds(numHits: Int) {
    import scala.collection.JavaConversions._

    val popularSeeds = overflow.keySet.toList.filter(seed => overflow.get(seed).size() >= numHits)
    
    popularSeeds.foreach(seed => {
      println(DNA.longToString(seed, seedLen) + " occurs at positions " + overflow.get(seed))
    })
  }
  
  /**
   * Given the target # of occurrences and the desired context length, choose a seed that occurs within k of the target #.
   * For each of its occurrences, print the position & context (prefix & suffix of indicated length).
   */
  def getSeedWithContext(genome: Genome, targetNumHits: Int, contextLen: Int, hitDiff: Int = 5) {
    import scala.collection.JavaConversions._

    var seed = 0L
    var numHits = 0
    
    breakable {
      overflow.keySet.toList.foreach(s => {
        val h = overflow.get(s).size()
        if (h >= (targetNumHits - hitDiff) && h <= (targetNumHits + hitDiff)) {
          seed = s
          numHits = h
          break()
        }
      })
    }
    
    println("Seed " + DNA.longToString(seed, seedLen))
    println("occurs " + numHits + " times")
    println("in the following contexts:")

    val hits = overflow.get(seed)
    hits.foreach(hitPos => {
      if (hitPos > contextLen) {
        println(genome.substring(hitPos - contextLen, hitPos + seedLen + contextLen) + " at " + hitPos)
      }
    })
  }
  
  /**
   * Given a seed, find its occurrences (plus context) and compute an edit distance matrix among them.
   */
  /*
  def getSeedEditDistanceMatrix(genome: Genome, targetNumHits: Int, contextLen: Int, hitDiff: Int = 5):(Array[Array[Int]], List[Int]) = {
    import scala.collection.JavaConversions._

    var seed = 0L
    var numHits = 0
    val l = new Levenshtein
    
    // get seed that has desired # occurrences
    breakable {
      overflow.keySet.toList.foreach(s => {
        val h = overflow.get(s).size()
        if (h >= (targetNumHits - hitDiff) && h <= (targetNumHits + hitDiff)) {
          seed = s
          numHits = h
          break()
        }
      })
    }
    
    // get its occurrences + context; save the hit positions
    println("Seed " + DNA.longToString(seed, seedLen))
    println("occurs " + numHits + " times")
    println("in the following contexts:")
        
    val hits = overflow.get(seed)
    var occurrencesWithContext = List[String]()
    var hitPos = List[Int]()
    
    hits.foreach(h => {
      if (h > contextLen) {
        println(genome.substring(h - contextLen, h + seedLen + contextLen) + " at " + h)
        occurrencesWithContext = genome.substring(h - contextLen, h + seedLen + contextLen) :: occurrencesWithContext
        hitPos = h :: hitPos
      }
    })
    
    // create the edit distance matrix
    var editDistanceMatrix = Array.fill(numHits, numHits)(0)
    
    (0 until numHits).foreach(i => {
      (0 until numHits).foreach(j => {
        editDistanceMatrix(i)(j) = l.distance(occurrencesWithContext(i), occurrencesWithContext(j))
      })
    })

    // print matrix
    println
    println("Edit distance matrix:")
    editDistanceMatrix.foreach(row => {row.foreach(e => {print(e + " ")}); println})
    
    (editDistanceMatrix, hitPos)
  }
  */
  
  def printMatrix(m: Array[Array[Int]]) = {
    m.foreach(mRow => {
      mRow.foreach(entry => {
        print(entry + "\t")
      })
      println
    })
  }
}

class IntHashIndexBuilder(seedLen: Int, estimatedSize: Long = 1024) extends IndexBuilder(seedLen) {
  val index = new IntHashIndex(seedLen, estimatedSize)
  
  override def add(seed: Long, pos: Long): Unit = index.add(seed, pos)
  
  override def build(): Index = {index}
}
