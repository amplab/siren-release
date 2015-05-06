package siren.test

import org.scalatest._
import siren._

class ReadTest extends FunSuite {
  val id1 = "@chr22_42896542_42897283_?:?:?_?:?:?_PATERNAL_42931663_42932399_14044316"
  val data1 = "GTCTTGAGCCTCAGTTTCCTTTCCTGTAAAACCGGAGTAAAGAAACAGCCCGTTTCCTAAGCCTTTCACAAAGGCCAGAGGGGTGCCAAACCCTCACACAC"
  val quality1 = "FFGFG9G=;?FEGEG<GDGGG?GGGFFDGDG>GGGG;GFGEGG=FGFFGFG6GEGCFGF>2FBGGAFCGGFFGGF1EGD@EFGFGF@6$-E?F5/C@<+94"
  val r1 = new Read(id1.getBytes, data1.getBytes, quality1.getBytes)
  val r1Copy = new Read(id1.getBytes, data1.getBytes, quality1.getBytes)
  
  val quality2 = "GGGEGGF@GFEEGG<EF@FGCF9GDEGGF?<GFDGGF@GGGGC+FGGGGGFGFGDFCFGGBGG?FGFGGFGGGGG?BFG:GFGGGGGGGGFBEDCG@G-:F"
  val r2 = new Read(id1.getBytes, data1.getBytes, quality2.getBytes)
  
  val id2 = "@chr22_42950508_42951536_?:?:?_?:?:?_MATERNAL_42985815_42986846_657f851"
  val r3 = new Read(id2.getBytes, data1.getBytes, quality1.getBytes)
  
  val data2 = "GTTTGGCACCCCTCTGGCCTTTGGGAAAGGCTTAGGAAACGGGCTGTTTCTTTACTCCGGTTTTACAGGACAGGAAACTGAGGCTCAAGACGTTTTAGTAA"
  val r4 = new Read(id1.getBytes, data2.getBytes, quality1.getBytes)
  
  test("should say yes if reads have the same ID, data, and quality") {
    assert(r1 == r1Copy)
  }
  
  test("should say no if reads have different quality") {
    assert(r1 != r2)
  }

  test("should say no if reads have different ID") {
    assert(r1 != r3)
  }
  
  test("should say no if reads have different data") {
    assert(r1 != r4)
  }
  
}