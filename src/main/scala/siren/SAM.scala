/*

Authors:
    Matei Zaharia & Kristal Curtis
    Developed as part of the SNAP project (http://snap.cs.berkeley.edu/)

*/

package siren

import scala.io.Source
import java.util.concurrent.ConcurrentHashMap
import java.io.BufferedOutputStream
import java.io.FileOutputStream

object SAM {
  def parseEntry(line: String): SAMEntry = {
    val fields = Utils.split(line, '\t')
    new SAMEntry(
      fields(0),                                          // read ID
      fields(1).toInt,                                    // flags
      fields(2),                                          // piece
      fields(3).toInt,                                    // position
      fields(4).toInt,                                    // map quality
      fields(5),                                          // cigar
      fields(6),                                          // next piece
      fields(7).toInt,                                    // next position
      if (fields(2) == fields(6)) fields(8).toInt else 0, // template len
      fields(9),                                          // sequence
      fields(10),                                         // quality
      new SAMTags(fields.slice(11, fields.length).toList) // tags
    )
  }

  def read(file: String): Iterator[SAMEntry] = {
    val lines = Source.fromFile(file).getLines
    lines.filter(!_.startsWith("@")).map(parseEntry)
  }
  
  def header(file: String): Iterator[String] = {
    val lines = Source.fromFile(file).getLines
    lines.filter(_.startsWith("@"))
  }
  
  def getFirstEntry(file: String): SAMEntry = {
    val lines = Source.fromFile(file).getLines
    val i = lines.filter(!_.startsWith("@")).map(parseEntry)
    if (!lines.isEmpty) i.next
    else null
  }

  def writer(file: String) = new Writer(file)
  
  class Writer(file: String) {
    private val out = new BufferedOutputStream(new FileOutputStream(file))
    
    def writeHeader(header: Iterator[String]): Unit = {
      header.foreach(h => {
        out.write(h.getBytes)
        out.write('\n')
      })
    }
    
    def write(entry: SAMEntry): Unit = {
      out.write(entry.toSAMLine.getBytes)
      out.write('\n')
    }
    
    def close() {
      out.close()
    }
  }

  // Bits in the flags field
  // Bits relating to pairs: 0x1, 0x2, 0x8, 0x20, 0x40, 0x80
  val MULTIPLE_SEGMENTS = 0x1 // relates to pairs
  val EACH_SEGMENT_PROPERLY_ALIGNED = 0x2 // relates to pairs
  val UNMAPPED = 0x4
  val MATE_UNMAPPED = 0x8 // relates to pairs
  val REVERSE = 0x10
  val MATE_REVERSE = 0x20 // relates to pairs
  val FIRST = 0x40 // relates to pairs
  val LAST = 0x80 // relates to pairs
  val SECONDARY_ALIGNMENT = 0x100
  val LOW_QUALITY = 0x200
  val DUPLICATE = 0x400
  val SUPPLEMENTARY_ALIGNMENT = 0x800
  val pairFlags = List(MULTIPLE_SEGMENTS, EACH_SEGMENT_PROPERLY_ALIGNED, MATE_UNMAPPED, MATE_REVERSE, FIRST, LAST)
}
