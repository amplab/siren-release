package siren

abstract class IndexBuilder(val seedLen: Int) {
  /** Add an entry to the index */
  def add(seed: Long, pos: Long): Unit
  
  /** Return the finished index */
  def build(): Index
}