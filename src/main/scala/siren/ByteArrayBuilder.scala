/*

Author:
    Matei Zaharia
    Developed as part of the SNAP project (http://snap.cs.berkeley.edu/)

*/

package siren

import java.util.Arrays
import scala.math.max

/**
 * Like a StringBuilder but for byte arrays.
 */
class ByteArrayBuilder(initialCapacity: Int = 16) {
  private var array = new Array[Byte](max(initialCapacity, 1))
  private var size = 0

  def append(byte: Byte): Unit = {
    if (size == array.length)
      array = Arrays.copyOf(array, array.length * 2)
    array(size) = byte
    size += 1
  }

  def append(bytes: Array[Byte]): Unit = {
    while (size + bytes.length - 1 >= array.length)
      array = Arrays.copyOf(array, array.length * 2)
    System.arraycopy(bytes, 0, array, size, bytes.length)
    size += bytes.length
  }

  def toArray: Array[Byte] = {
    Arrays.copyOf(array, size)
  }

  def clear(): Unit = { 
    size = 0
  }
}
