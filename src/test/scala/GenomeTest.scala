package siren.test

import org.scalatest._
import siren._

class GenomeTest extends FunSuite with BeforeAndAfter {
  test("get non-N size") {
    // create dummy genome file
    val genomeFile = "dummy.fa"
    val fw = new java.io.FileWriter(genomeFile)
    val bw = new java.io.BufferedWriter(fw)
    bw.write(">chr1")
    bw.newLine
    bw.write("NNNNNNNNNNNNNNN")
    bw.newLine
    bw.write("ACTGACTNNACTGAN")
    bw.newLine
    bw.write(">chr2")
    bw.newLine
    bw.write("CCGAAANNACATGAN")
    bw.newLine
    bw.write("NNNNNNNNNNNNNNN")
    bw.newLine
    bw.close
    
    // load genome
    val genome = FASTA.read(genomeFile)
    
    // get non-N size
    assert(genome.nonNSize == 24)
  }
}