package siren

import scala.math._

object Wgsim {
  val ID_REGEX = """@*([^:]+)_(\d+)_(\d+)_.*""".r
  
  def getChr(id: String): String = {
    var chr = ""
    id match {
      case ID_REGEX(piece, start, end) => chr = piece
    }
    chr
  }
  
  def getLowEnd(id: String): Int = {
    var lowEnd = 0
    id match {
      case ID_REGEX(piece, start, end) => lowEnd = min(start.toInt, end.toInt)
    }
    lowEnd
  }
  
  def getHighEnd(id: String): Int = {
    var highEnd = 0
    id match {
      case ID_REGEX(piece, start, end) => highEnd = max(start.toInt, end.toInt)
    }
    highEnd
  }
  
  def isCorrectDeprecated(id: String, pos: Long, maxDist: Int, zeroIndexing: Boolean = true): Boolean = {
    val posToCheck = 
      if (zeroIndexing) pos + 1
      else pos
    
    val (myPiece, myOffset) = GenomeLoader.genome.getLocation(posToCheck)
    
    (myPiece == getChr(id) && myOffset >= getLowEnd(id) - maxDist && myOffset <= getHighEnd(id) + maxDist)
  }
  
  def isCorrect(e: SAMEntry, slack: Int = 0 /* allow some slack around the endpoints */): Boolean = {
    val lowEnd = getLowEnd(e.readId)
    val highEnd = getHighEnd(e.readId) - e.readLen + 1
    //(e.piece == getChr(e.readId) && e.position >= (getLowEnd(e.readId) - slack) && e.position <= (getHighEnd(e.readId) + slack))
    e.piece == getChr(e.readId) &&
    /*
    ((e.position >= (lowEnd - slack) && e.position <= (lowEnd + slack)) // aligns within slack of low end
    || ((e.position >= (highEnd - slack) && e.position <= (highEnd + slack)))// aligns within slack of high end
    */
    (abs(e.position - lowEnd) <= slack || abs(e.position - highEnd) <= slack)
  }
  
  def getWgsimId(originalId: String, pos1: Long, pos2: Long, zeroIndexing: Boolean = true): String = {
    val (myPiece1, myOffset1) = GenomeLoader.genome.getLocation(pos1)
    val (myPiece2, myOffset2) = GenomeLoader.genome.getLocation(pos2)
    
    val lowEnd = 
      if (zeroIndexing)
        min(myOffset1, myOffset2) + 1
      else
        min(myOffset1, myOffset2)
      
    val highEnd = 
      if (zeroIndexing)
        max(myOffset1, myOffset2) + 1
      else
        max(myOffset1, myOffset2)
    
    if (myPiece1 == myPiece2)
      List(myPiece1, lowEnd, highEnd, 0 + ":" + originalId).mkString("_")
    else
      null
  }
}