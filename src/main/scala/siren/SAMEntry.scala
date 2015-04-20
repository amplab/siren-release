package siren

class SAMEntry(
    val readId: String,
    val flags: Int,
    val piece: String, 
    val position: Int,
    val mapQuality: Int,
    val cigar: String,
    val nextPiece: String,
    val nextPosition: Int,
    val templateLen: Int,
    val sequence: String,
    val quality: String,
    val tags: SAMTags) {
  override def toString(): String = readId

  def toSAMLine: String = List(readId, flags, piece, position, mapQuality, cigar, nextPiece, nextPosition, templateLen, sequence, quality).mkString("\t") + "\t" + tags.toString

  def direction = if ((flags & SAM.REVERSE) != 0) 1 else 0

  def reversed: Boolean = ((flags & SAM.REVERSE) != 0)
  
  def paired: Boolean = ((flags & SAM.MULTIPLE_SEGMENTS) != 0)
  
  def unmapped: Boolean = ((flags & SAM.UNMAPPED) != 0)
  
  def readLen = sequence.length

  def readIdWithoutSuffix: String = {
    if (readId.endsWith("/1") || readId.endsWith("/2")) {
      readId.split("/")(0)
    } else {
      readId
    }
  }
  
  def toRead = {
    if (reversed) new Read(readId.getBytes, DNA.rc(sequence).getBytes, quality.reverse.getBytes)
    else new Read(readId.getBytes, sequence.getBytes, quality.getBytes)
  }
  
  def toReadWithSuffix(whichRead: Int) = {
    val id = readId + "/" + whichRead
    if (reversed) new Read(id.getBytes, DNA.rc(sequence).getBytes, quality.reverse.getBytes)
    else new Read(id.getBytes, sequence.getBytes, quality.getBytes)
  }
  
  def hasSameAlignmentAs(that: SAMEntry): Boolean = {
    piece == that.piece && position == that.position
  }
  
  /*
  def forceToUnpaired(idSuffix: String = ""): SAMEntry = {
    // return new SAMEntry, discarding info about its mate
    // that means updating the id (with a suffix), flags, nextPiece, nextPosition, & templateLen fields
    
    // Bits relating to pairs: 0x1, 0x2, 0x8, 0x20, 0x40, 0x80 => set them all to 0
    // ** could be incorrect **
    var newFlags = flags
    SAM.pairFlags.foreach(f => {
      if ((flags & f) != 0) newFlags -= f
    })
    
    new SAMEntry(
      readId + idSuffix,
      newFlags,
      piece, 
      position,
      mapQuality,
      cigar,
      "*",
      0,
      0,
      sequence,
      quality,
      new SAMTags(List(tags.getTag("RG").toString)) // remove tags besides read group
    )
  }

  def updateAlignment(res: AlignResult, genome: Genome): SAMEntry = {
    res match {
      case RichSingleHit(absPos, isRC, editDistance) => {
        val (truePiece, truePos) = genome.getLocation(absPos)

        val newFlags = 
          if (isRC) SAM.REVERSE
          else 0

        val newSequence = 
          if (isRC) DNA.rc(sequence)
          else sequence

        val newQuality =
          if (isRC) quality.reverse
          else quality
        
        // create new sam entry with updated alignment
        new SAMEntry(
          readId,
          newFlags,
          truePiece,
          truePos + 1, // because true position is 0-indexed, but SAM should be 1-indexed
          60, // dummy value
          readLen + "M", // cigar.toString
          "*",  // assume unpaired
          0,  // assume unpaired
          0,  // assume unpaired
          newSequence,
          newQuality,
          new SAMTags(List(tags.getTag("RG").toString)) //, "MD:Z:" + mdTag.toString))  // keep read group of original
        )
      } case _ => {
        println("Cannot update alignment.")
        this
      }
    }
  }

  // sets to unmapped; if reverse aligned (and therefore sequence is RC, quality is reversed), will reset sequence & quality
  // keep read group
  def removeAlignment: SAMEntry = {
    val newSequence = 
      if (reversed) DNA.rc(sequence)
      else sequence
    
    val newQuality =
      if (reversed) quality.reverse
      else quality
    
    val t = tags.getTag("RG")
    val newTags = 
      if (t == null) new SAMTags(Nil)
      else new SAMTags(List(t.toString))
    new SAMEntry(
      readId,
      0,  // remove flags
      "*",  // do not specify piece
      0,  // do not specify position
      0,  // do not specify mapq
      "*",  // do not specify cigar
      "*",  // do not specify next piece
      0,  // do not specify next position
      0,  // do not specify template len
      newSequence,
      newQuality,
      newTags
    )
  }

  def contigToChrPos: SAMEntry = {
    val contigRange = """(\w+):(\d+)-(\d+)""".r
    // fix piece & position -- initially, it's formatted as a range & offset, e.g. "chr22:42950584-42955394     2980" 
    // new format will be piece & position as usual, like "chr22 42953564"
    try{
      val contigRange(newPiece, startStr, endStr) = piece
      val offset = position
      val newEntry = new SAMEntry(
        readId,                   // read ID
        flags,                    // flags
        newPiece,                 // piece
        startStr.toInt + offset,  // position
        mapQuality,               // map quality
        cigar,                    // cigar
        nextPiece,                // next piece
        nextPosition,             // next position
        templateLen,              // template len
        sequence,                 // sequence
        quality,                  // quality
        tags
      )
      newEntry
    } catch {
      case error: MatchError => println("Match error: " + piece + ", " + position)
      this
    }
  }

  // return new entry with all data same as current entry, except use a sanitized CIGAR (e.g., "101M")
  // will also update tags:  only retains RG tag (to avoid confusion with NM or MD tags); all other tags are dropped
  def sanitizeCigar: SAMEntry = {
    new SAMEntry(
      readId,
      flags,
      piece, 
      position,
      mapQuality,
      readLen + "M",  // new dummy cigar string
      nextPiece,
      nextPosition,
      templateLen,
      sequence,
      quality,
      new SAMTags(List(tags.getTag("RG").toString)) // remove tags besides read group
    )
  }
  */
}
