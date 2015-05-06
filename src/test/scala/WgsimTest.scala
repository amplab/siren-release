package siren.test

import org.scalatest._
import siren._
import scala.math._

class WgsimTest extends FunSuite {
  val id = "@chr22_20910748_20911243_0:0:0_0:0:0_2/1"
  val maxDist = 10

  test("get chromosome from wgsim id") {
    val chr = Wgsim.getChr(id)
    assert(chr == "chr22")
  }
  
  test("get lower position from wgsim id") {
    val lowerPos = Wgsim.getLowEnd(id)
    assert(lowerPos == 20910748)
  }
  
  test("get higher position from wgsim id") {
    val higherPos = Wgsim.getHighEnd(id)
    assert(higherPos == 20911243)
  }
  
  ignore("score position pair when answer should be correct") {
    val pos1 = GenomeLoader.genome.getPiece("chr22").startIndex + 20910748
    val pos2 = GenomeLoader.genome.getPiece("chr22").startIndex + 20911173
    assert(Wgsim.isCorrectDeprecated(id, pos1, maxDist))
    assert(Wgsim.isCorrectDeprecated(id, pos2, maxDist))
  }
  
  ignore("score position pair when answer should be incorrect") {
    val pos = GenomeLoader.genome.getPiece("chr22").startIndex + 10911173
    assert(!Wgsim.isCorrectDeprecated(id, pos, maxDist))
  }

  test("score sam entry when answer should be correct") {
    val e = new SAMEntry(
        id,
        0,
        "chr22", 
        20910748,
        0,
        "",
        "",
        0,
        0,
        "",
        "",
        new SAMTags(Nil))
    
    assert(Wgsim.isCorrect(e))
  }
  
  test("score sam entry when answer should be incorrect") {
    val e = new SAMEntry(
        id,
        0,
        "chr22", 
        0,
        0,
        "",
        "",
        0,
        0,
        "",
        "",
        new SAMTags(Nil))
    
    assert(!Wgsim.isCorrect(e))
  }
  
  ignore("get wgsim-style id given zero-indexed position pair") {
    val pos1 = GenomeLoader.genome.getPiece("chr22").startIndex + 20910747
    val pos2 = GenomeLoader.genome.getPiece("chr22").startIndex + 20911242
    val originalId = "originalId"

    assert(Wgsim.getWgsimId(originalId, pos1, pos2) == "@chr22_20910748_20911243_0:originalId")
  }
  
  ignore("get wgsim-style id given 1-indexed position pair") {
    val pos1 = GenomeLoader.genome.getPiece("chr22").startIndex + 20910748
    val pos2 = GenomeLoader.genome.getPiece("chr22").startIndex + 20911243
    val originalId = "originalId"

    assert(Wgsim.getWgsimId(originalId, pos1, pos2, false) == "@chr22_20910748_20911243_0:originalId")
  }
}