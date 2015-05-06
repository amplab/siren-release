package siren.test

import org.scalatest._
import siren._

class SAMTagsTest extends FunSuite {
  val tagStr = "XC:i:100	XT:A:U	NM:i:0	SM:i:29	AM:i:29	X0:i:1	X1:i:0	XM:i:0	XO:i:0	XG:i:0	MD:Z:100	RG:Z:VenterC"
  val tagList = tagStr.split("\t").toList
  val samTags = new SAMTags(tagList)
  
  test("should initialize correctly") {
    assert(samTags.tagsMap.keySet.size == tagList.length)
    assert(samTags.tagsMap.get("XC").get == SAMTag("XC", "i", "100"))
    assert(samTags.tagsMap.get("XT").get == SAMTag("XT", "A", "U"))
    assert(samTags.tagsMap.get("NM").get == SAMTag("NM", "i", "0"))
    assert(samTags.tagsMap.get("SM").get == SAMTag("SM", "i", "29"))
    assert(samTags.tagsMap.get("AM").get == SAMTag("AM", "i", "29"))
    assert(samTags.tagsMap.get("X0").get == SAMTag("X0", "i", "1"))
    assert(samTags.tagsMap.get("X1").get == SAMTag("X1", "i", "0"))
    assert(samTags.tagsMap.get("XM").get == SAMTag("XM", "i", "0"))
    assert(samTags.tagsMap.get("XO").get == SAMTag("XO", "i", "0"))
    assert(samTags.tagsMap.get("XG").get == SAMTag("XG", "i", "0"))
    assert(samTags.tagsMap.get("MD").get == SAMTag("MD", "Z", "100"))
    assert(samTags.tagsMap.get("RG").get == SAMTag("RG", "Z", "VenterC"))
  }
  
  test("to string") {
    val s = samTags.toString
    val l = s.split("\t").toList
    assert(tagStr.split("\t").toList.sortWith(_ < _) == l.sortWith(_ < _))
  }
  
  test("should return tag if it exists") {
    assert(samTags.getTag("XC") == SAMTag("XC", "i", "100"))
  }
  
  test("should return null & print a warning if tag doesn't exist") {
    assert(samTags.getTag("badTagName") == null)
  }
  
  test("should add new tag if it doesn't already exist") {
    val t = "AA:Z:100"
    samTags.addTag(t)
    assert(samTags.getTag("AA") == SAMTag.getSAMTag(t))
  }
  
  test("should print a warning if tag already exists & you try to add it again") {
    val t = "RG:Z:VenterC"
    samTags.addTag(t)
  }
  
  test("should overwrite a tag if it's there") {
    val t = "RG:Z:FASTQ"
    val tag = SAMTag.getSAMTag(t)
    samTags.addOrOverwriteTag(tag)
    assert(samTags.getTag("RG") == tag)
  }
  
  test("should add tag if tag to be overwritten isn't there") {
    val tagStr = "XC:i:100	XT:A:U"
    val tagList = tagStr.split("\t").toList
    val samTags = new SAMTags(tagList)
    val t = "RG:Z:FASTQ"
    val tag = SAMTag.getSAMTag(t)
    samTags.addOrOverwriteTag(tag)
    assert(samTags.getTag("RG") == tag)
  }
}