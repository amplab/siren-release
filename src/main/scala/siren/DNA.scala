package siren

/**
 * Utilities for manipulating DNA strings.
 */
object DNA {
  val A: Byte = 0
  val C: Byte = 1
  val G: Byte = 2
  val T: Byte = 3

  val BASE_TO_CODE = new Array[Byte](128)
  BASE_TO_CODE('A') = 0
  BASE_TO_CODE('C') = 1
  BASE_TO_CODE('G') = 2
  BASE_TO_CODE('T') = 3

  val CODE_TO_BASE = Array[Char]('A', 'C', 'G', 'T')

  val COMPLEMENT = new Array[Byte](128)
  COMPLEMENT('A') = 'T'
  COMPLEMENT('C') = 'G'
  COMPLEMENT('G') = 'C'
  COMPLEMENT('T') = 'A'
  COMPLEMENT('N') = 'N'

  def reverseComplement(s: String): String = {
    var buf = new Array[Char](s.length)
    var i = 0
    while (i < s.length) {
      buf(s.length - i - 1) = COMPLEMENT(s.charAt(i)).toChar
      i += 1
    }
    new String(buf)
  }
  
  def rc(s: String): String = reverseComplement(s)

  def getReverseComplement(str: Array[Byte], out: Array[Byte]) {
    var i = 0
    while (i < str.length) {
      out(str.length - i - 1) = COMPLEMENT(str(i))
      i += 1
    }
  }

  def longToString(n: Long, len: Int): String = {
    if (len > 32)
      throw new IllegalArgumentException("Invalid length (bigger than 32)")
    val buf = new Array[Char](len)
    var nn = n
    for (i <- 0 until len) {
      buf(len - 1 - i) = CODE_TO_BASE(nn.toInt & 3)
      nn >>= 2
    }
    new String(buf)
  }

  def stringToLong(s: String): Long = {
    substringToLong(s, 0, s.length)
  }

  def substringToLong(s: String, start: Int, end: Int): Long = {
    if (end - start > 32)
      throw new IllegalArgumentException("Substring longer than 32 bases")
    var ans = 0L
    var i = start
    while (i < end) {
      ans = (ans << 2) | BASE_TO_CODE(s.charAt(i))
      i += 1
    }
    ans
  }

  def rcSubstringToLong(s: String, start: Int, end: Int): Long = {
    if (end - start > 32)
      throw new IllegalArgumentException("Substring longer than 32 bases")
    var ans = 0L
    var i = (s.length - 1 - start)
    while (i > (s.length - 1 - end)) {
      ans = (ans << 2) | (3 - BASE_TO_CODE(s.charAt(i)))
      i -= 1
    }
    ans
  }

  def substringToLong(s: Array[Byte], start: Int, end: Int): Long = {
    if (end - start > 32)
      throw new IllegalArgumentException("Substring longer than 32 bases")
    var ans = 0L
    var i = start
    while (i < end) {
      ans = (ans << 2) | BASE_TO_CODE(s(i))
      i += 1
    }
    ans
  }

  def rcSubstringToLong(s: Array[Byte], start: Int, end: Int): Long = {
    if (end - start > 32)
      throw new IllegalArgumentException("Substring longer than 32 bases")
    var ans = 0L
    var i = (s.length - 1 - start)
    while (i > (s.length - 1 - end)) {
      ans = (ans << 2) | (3 - BASE_TO_CODE(s(i)))
      i -= 1
    }
    ans
  }
}
