/*

Author:
    Matei Zaharia
    Developed as part of the SNAP project (http://snap.cs.berkeley.edu/)

*/

package siren

import it.unimi.dsi.fastutil.longs.LongList
import it.unimi.dsi.fastutil.longs.LongArrayList

/** An insert-only index for occurrences of seeds in DNA */
abstract class Index(val seedLen: Int) {
  /**
   * Get up to maxResults positions of a given seed into the given list and
   * return true if the total number of positions is less than or equal to
   * maxResults, or otherwise return false and get no results.
   */
  def get(seed: Long, dest: LongList, maxResults: Int): Boolean
  
  /**
   * Get up to maxResults positions of a given seed as a new list.
   */
  def getList(seed: Long, maxResults: Int): LongList = {
    val results = new LongArrayList()
    get(seed, results, maxResults)
    results
  }
}
