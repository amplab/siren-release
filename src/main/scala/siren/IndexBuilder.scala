/*

Author:
    Matei Zaharia
    Developed as part of the SNAP project (http://snap.cs.berkeley.edu/)

*/

package siren

abstract class IndexBuilder(val seedLen: Int) {
  /** Add an entry to the index */
  def add(seed: Long, pos: Long): Unit
  
  /** Return the finished index */
  def build(): Index
}