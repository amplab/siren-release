/*

Author:
    Matei Zaharia
    Developed as part of the SNAP project (http://snap.cs.berkeley.edu/)

*/

package siren

object GenomeLoader {
  // TODO: fill in path to genome file
  lazy val genome = FASTA.read("~/hg19.fa")
}