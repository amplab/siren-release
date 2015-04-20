package siren

import scala.collection.mutable.ArrayBuffer

object Utils {
  /** Count how many bytes in the array are equal to a given value */
  def count(bytes: Array[Byte], value: Byte): Int = {
    var i = 0
    var count = 0
    while (i < bytes.length) {
      if (bytes(i) == value)
        count += 1
      i += 1
    }
    count
  }

  /** Split a string around instances of a given delimiter */
  def split(s: String, delimiter: Char): Seq[String] = {
    val buf = new ArrayBuffer[String]
    var i = 0
    while (i < s.length) {
      var j = i
      while (j < s.length && s.charAt(j) != delimiter) {
        j += 1
      }
      if (j > i) {
        buf += s.substring(i, j);
      }
      i = j
      while (i < s.length && s.charAt(i) == delimiter) {
        i += 1
      }
    }
    return buf
  }

  /*
  // Check whether a SNP in a child could have been inherited from two parents
  def mendelOK(child: SNP, mom: SNP, dad: SNP): Boolean = {
    ((mom.contains(child.allele1) && dad.contains(child.allele2)) ||
     (dad.contains(child.allele1) && mom.contains(child.allele2)))
  }

  // Check whether the differences between a child and parents are OK in terms of inheritance;
  // prints a report on the differences to stdout
  def scoreTrio(
      child: SimpleVC,
      mom: SimpleVC,
      dad: SimpleVC,
      start: Int = 0,
      end: Int = Int.MaxValue,
      onlyPrintBad: Boolean = false)
  {
    val locs = getDifferenceLocs(child, mom, dad, start, end)
    println("%15s %5s %5s".format("child", "mom", "dad"))
    var numOk = 0
    var numUnsure = 0
    var numBad = 0
    for (loc <- locs) {
      val (childSnp, momSnp, dadSnp) = (child.snps(loc), mom.snps(loc), dad.snps(loc))
      val ok = mendelOK(childSnp, momSnp, dadSnp)
      val oneUncalled = childSnp.uncalled || momSnp.uncalled || dadSnp.uncalled
      var mark = "-"
      if (ok) {
        mark = "-"
        numOk += 1
      } else if (oneUncalled) {
        mark = "?"
        numUnsure += 1
      } else {
        mark = "EEK"
        numBad += 1
      }
      if (!onlyPrintBad || mark == "EEK") {
        println("%8d:%6s%6s%6s%6s".format(loc, childSnp, momSnp, dadSnp, mark))
      }
    }
    printf("Total: %d, OK: %d, unsure: %d, bad: %d\n", locs.size, numOk, numUnsure, numBad)
  }

  // Get the list of locations where any of the genomes in a trio differs from the reference
  def getDifferenceLocs(
      child: SimpleVC,       
      mom: SimpleVC,         
      dad: SimpleVC,         
      start: Int = 0,        
      end: Int = Int.MaxValue): Seq[Int] =
  {
    def locsInRange(vc: SimpleVC) = {
      vc.diffList.map(_._1).filter(x => x >= start && x < end)
    }
    (locsInRange(child) ++ locsInRange(mom) ++ locsInRange(dad)).toSet.toArray.sorted
  }

  def parsePhred(score: Char): Int = score - 33

  def parsePhred(score: Byte): Int = score - 33

  def scoreSNPs(vc: SimpleVC, trueSNPs: Seq[(Int, SNP)], start: Int = 0, end: Int = Int.MaxValue) {
    val calledDiffs = vc.diffList.filter(p => p._1 >= start && p._1 < end).toSet
    val trueDiffs = trueSNPs.filter(p => p._1 >= start && p._1 < end).toSet
    val calledDiffsMap = calledDiffs.toMap
    val trueDiffsMap = trueDiffs.toMap
    val falsePositiveLocs = calledDiffs.map(_._1) -- trueDiffs.map(_._1)  // Set of locations we called wrong
    val falseNegativeLocs = trueDiffs.map(_._1) -- calledDiffs.map(_._1)  // Set of locations we missed
    val wronglyCalled = calledDiffs.filter(p => trueDiffsMap.contains(p._1) && trueDiffsMap(p._1) != p._2)
    val falseNegativesSkipped = falseNegativeLocs.filter(l => vc.snps(l).uncalled).size
    val badFalseNegatives = falseNegativeLocs.size - falseNegativesSkipped
    println("False negatives:")
    falseNegativeLocs.toSeq.sorted.foreach(l => printf("%9d %5s %5s\n", l, vc.snps(l), trueDiffsMap(l)))
    println("False negatives not skipped:")
    falseNegativeLocs.toSeq.sorted.filter(l => !vc.snps(l).uncalled)
        .foreach(l => printf("%9d %5s %5s\n", l, vc.snps(l), trueDiffsMap(l)))
    println("False positives:")
    falsePositiveLocs.toSeq.sorted.foreach(l => printf("%9d %5s %5s\n", l, calledDiffsMap(l), "-"))
    println("Wrongly called:")
    wronglyCalled.toSeq.sortBy(_._1).foreach(p => printf("%9d %5s %5s\n", p._1, p._2, trueDiffsMap(p._1)))
    printf("Total: %d, false negatives: %d (%d non-skipped), false positives: %d, wrongly called: %d\n",
        trueDiffs.size, falseNegativeLocs.size, badFalseNegatives, falsePositiveLocs.size, wronglyCalled.size)
  }
  */
}
