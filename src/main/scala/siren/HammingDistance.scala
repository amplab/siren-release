package snap

object HammingDistance {
  /**
   * Return the Hamming distance between two strings, or -1 if it exceeds maxDistance.
   */
  def distance(s1: Array[Byte], s2: Array[Byte], maxDistance: Int): Int = {
    if (s1.length != s2.length) {
      throw new IllegalArgumentException("Arrays not of equal size")
    }
    var distance = 0
    var i = 0
    while (i < s1.length) {
      if (s1(i) != s2(i)) {
        distance += 1
        if (distance > maxDistance) {
          return -1
        }
      }
      i += 1
    }
    return distance
  }

  /**
   * Return the Hamming distance between two substrings of tow arrays, or -1 if it exceeds maxDistance.
   */
  def distance(s1: Array[Byte], off1: Int, s2: Array[Byte], off2: Int, length: Int, maxDistance: Int): Int = {
    if (off1 < 0 || off2 < 0 || length < 0 || off1 + length > s1.length || off2 + length > s2.length) {
      throw new IllegalArgumentException("Invalid array bounds")
    }
    var distance = 0
    var i1 = off1
    var i2 = off2
    var end1 = off1 + length
    while (i1 < end1) {
      if (s1(i1) != s2(i2)) {
        distance += 1
        if (distance > maxDistance) {
          return -1
        }
      }
      i1 += 1
      i2 += 1
    }
    return distance
  }
}
