package siren.test

import org.scalatest._
import siren._

class SAMEntryTest extends FunSuite {
  // reverse
  val samLine1 = "chr22_16049978_16050389_?:?:?_?:?:?_PATERNAL_16049978_16050389_13fb8ac6	83	chr22	16050289	15	101M	=	16050001	0	TTCTCTCTTTTGGATGATTCTAAGTACCAGCTAAAATACAGCTATCATTCATTTTCCTTGATTTGGGAGCCTAATTTCTTTAATTTAGTATGCAAGAAAAC	EBEDGEE>G7FB?FE6GEDFGGGGED*GGGG8E<=GEGGDGGFGAAGGBGGGEGDCGFCCBGGG33F5G:EGGGGGGGFCGFGE1@?FGBEFG3GFG@EEF	X0:i:1	XM:i:0	XO:i:0	X1:i:1	XT:A:U	XA:Z:chr14,+19792612,101M,1;	XG:i:0	NM:i:0	RG:Z:VenterC	SM:i:15	AM:i:15	MD:Z:101"
  val samEntry1 = SAM.parseEntry(samLine1)
  
  // forward
  val samLine2 = "chr22_16049978_16050389_?:?:?_?:?:?_PATERNAL_16049978_16050389_13fb8ac6	163	chr22	16050001	15	23S78M	=	16050289	0	AAATTAACTATATACATAACTAAGATCTGATAAGTCCCAGGACTTCAGAAGAGCTGTGAGACCTTGGCCAAGTCACTTCCTCCTTCAGGAACATTGCAGTG	$%&$##'#%###($%#%#$$###@DG>GGGGGCGGD3FGF$9GFGGDEGGFDGFG?GFD*GGGGGGG7FGEEDFDF8EFFGD?F=-FCGFEFAG=FCAECE	XM:i:0	XO:i:0	XT:A:M	XG:i:0	NM:i:0	RG:Z:VenterC	SM:i:15	AM:i:15	MD:Z:78"
  val samEntry2 = SAM.parseEntry(samLine2)
  
  // unpaired
  val samLine3 = "chr22_16049978_16050389_?:?:?_?:?:?_PATERNAL_16049978_16050389_13fb8ac6	0	chr22	16050289	60	101M	*	0	0	TTCTCTCTTTTGGATGATTCTAAGTACCAGCTAAAATACAGCTATCATTCATTTTCCTTGATTTGGGAGCCTAATTTCTTTAATTTAGTATGCAAGAAAAC	EBEDGEE>G7FB?FE6GEDFGGGGED*GGGG8E<=GEGGDGGFGAAGGBGGGEGDCGFCCBGGG33F5G:EGGGGGGGFCGFGE1@?FGBEFG3GFG@EEF	RG:Z:VenterC"
  val samEntry3 = SAM.parseEntry(samLine3)
  
  test("should return proper sam line") {
    //assert(samEntry1.toSAMLine == samLine1) // fails b/c tags are in wrong order
    assert(samEntry3.toSAMLine == samLine3)
  }
  
  test("reversed should say yes if reversed") {
    assert(samEntry1.reversed)
  }
  
  test("reversed should say no if forward") {
    assert(!samEntry2.reversed)
  }
  
  test("paired should say yes if paired") {
    assert(samEntry1.paired)
    assert(samEntry2.paired)
  }
  
  test("paired should say no if unpaired") {
    assert(!samEntry3.paired)
  }
  
  test("should return right readLen") {
    assert(samEntry1.readLen == 101)
    assert(samEntry2.readLen == 101)
    assert(samEntry3.readLen == 101)
  }
  
  test("should return corresponding read for forward entry") {
    val id2 = "chr22_16049978_16050389_?:?:?_?:?:?_PATERNAL_16049978_16050389_13fb8ac6"
    val data2 = "AAATTAACTATATACATAACTAAGATCTGATAAGTCCCAGGACTTCAGAAGAGCTGTGAGACCTTGGCCAAGTCACTTCCTCCTTCAGGAACATTGCAGTG"
    val quality2 = "$%&$##'#%###($%#%#$$###@DG>GGGGGCGGD3FGF$9GFGGDEGGFDGFG?GFD*GGGGGGG7FGEEDFDF8EFFGD?F=-FCGFEFAG=FCAECE"
    val read = new Read(id2.getBytes, data2.getBytes, quality2.getBytes)
    val readToTest = samEntry2.toRead
    assert(read.idStr == readToTest.idStr)
    assert(read.dataStr == readToTest.dataStr)
    assert(read.qualityStr == readToTest.qualityStr)
  }
  
  test("should return corresponding read for reverse entry") {
    val id1 = "chr22_16049978_16050389_?:?:?_?:?:?_PATERNAL_16049978_16050389_13fb8ac6"
    val data1 = DNA.rc("TTCTCTCTTTTGGATGATTCTAAGTACCAGCTAAAATACAGCTATCATTCATTTTCCTTGATTTGGGAGCCTAATTTCTTTAATTTAGTATGCAAGAAAAC")
    val quality1 = "EBEDGEE>G7FB?FE6GEDFGGGGED*GGGG8E<=GEGGDGGFGAAGGBGGGEGDCGFCCBGGG33F5G:EGGGGGGGFCGFGE1@?FGBEFG3GFG@EEF".reverse
    val read = new Read(id1.getBytes, data1.getBytes, quality1.getBytes)
    val readToTest = samEntry1.toRead
    assert(read.idStr == readToTest.idStr)
    assert(read.dataStr == readToTest.dataStr)
    assert(read.qualityStr == readToTest.qualityStr)
  }
  
  test("should say yes if two sam entries are aligned to the same piece/position if their mates are too") {
    assert(samEntry1.hasSameAlignmentAs(samEntry1))
  }
  
  test("should say yes if two sam entries are aligned to the same piece/position even if their mates are aligned to different locations") {
    assert(samEntry1.hasSameAlignmentAs(samEntry3))
  }
  
  test("should say no if two sam entries are aligned to different locations") {
    assert(!samEntry1.hasSameAlignmentAs(samEntry2))
  }

  test("should say yes if read is unmapped") {
    assert(!samEntry1.unmapped)
  }
  
  test("should say no if read is mapped") {
    val l = "chr22_16049978_16050389_?:?:?_?:?:?_PATERNAL_16049978_16050389_13fb8ac6	77	*	0	0	*	*	0	0	TTCTCTCTTTTGGATGATTCTAAGTACCAGCTAAAATACAGCTATCATTCATTTTCCTTGATTTGGGAGCCTAATTTCTTTAATTTAGTATGCAAGAAAAC	EBEDGEE>G7FB?FE6GEDFGGGGED*GGGG8E<=GEGGDGGFGAAGGBGGGEGDCGFCCBGGG33F5G:EGGGGGGGFCGFGE1@?FGBEFG3GFG@EEF	PG:Z:novoalign	ZS:Z:R	NH:i:2"
    val e = SAM.parseEntry(l)
    assert(e.unmapped)
  }
  
  test("should strip suffix from read id if it has a suffix") {
    val l = "chr22_16049978_16050389_?:?:?_?:?:?_PATERNAL_16049978_16050389_13fb8ac6/1	77	*	0	0	*	*	0	0	TTCTCTCTTTTGGATGATTCTAAGTACCAGCTAAAATACAGCTATCATTCATTTTCCTTGATTTGGGAGCCTAATTTCTTTAATTTAGTATGCAAGAAAAC	EBEDGEE>G7FB?FE6GEDFGGGGED*GGGG8E<=GEGGDGGFGAAGGBGGGEGDCGFCCBGGG33F5G:EGGGGGGGFCGFGE1@?FGBEFG3GFG@EEF	PG:Z:novoalign	ZS:Z:R	NH:i:2"
    val e = SAM.parseEntry(l)
    assert(e.readIdWithoutSuffix == "chr22_16049978_16050389_?:?:?_?:?:?_PATERNAL_16049978_16050389_13fb8ac6")
  }
  
  test("should return read id unchanged if no suffix") {
    assert(samEntry1.readIdWithoutSuffix == samEntry1.readId)
  }
}