/*

Authors:
    Matei Zaharia & Kristal Curtis
    Developed as part of the SNAP project (http://snap.cs.berkeley.edu/)

*/

package siren
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.math.{max, min}
import scala.util.control.Breaks.{break, breakable}

case class GenomePiece(name: String, data: Array[Byte], startIndex: Long) {
  override def toString = "Piece(" + name + "," + startIndex + ")"
  def endIndex = startIndex + data.length
  def size = data.length

  // TODO
  // update genome with alt bases of vcf entry
  // NOTE:  this may not work well if the genome has multiple pieces, but it should be fine for snippets/single-piece genomes
  /*
  def updateData(entries: List[VCFEntry]): Array[Byte] = {
    val minSVLength = 2 // because updating must be handled differently if variant is single vs. multiple bases
    var updatedData = data.clone

    entries.foreach(v => {
      // grab the array that precedes the insertion
      val (prefix, suffix) = updatedData splitAt v.position0Indexed
    
      // overwrite the ref base & add in the new bases
      val (ref, newSuffix) = suffix splitAt v.referenceBases.length
      
      // append it with the rest of the array
      updatedData = prefix ++ v.alternateBases.getBytes ++ newSuffix

    })

    updatedData
  }
  */
}
  
class Genome {
  //val INDEX_GAP: Long = 1000 // Gap in positions given to different pieces
  val INDEX_GAP: Long = 0 // to be compatible with index in CSNAP
  
  /*private*/ val _pieces = new ArrayBuffer[GenomePiece]
  
  def substring(start: Long, end: Long): String = {
    val buf = new Array[Byte]((end - start).toInt)
    getSubstring(start, end, buf)
    new String(buf)
  }

  def validSubstring(start: Long, end: Long): Boolean = {
    /*
    val str = substring(start, end)
    !str.contains('N')
    */
    val len = (end - start).toInt
    val buf = new Array[Byte](len)
    getSubstring(start, end, buf)
    
    var i = 0
    while (i < len) {
      if (buf(i) == 'N')
        return false
      
      i += 1
    }
    
    true
  }

  def getSubstring(start: Long, end: Long, dest: Array[Byte]): Unit = {
    var pieceId = 0
    while (start > _pieces(pieceId).endIndex && pieceId < _pieces.size)
      pieceId += 1
    if (pieceId > 0 && start > _pieces(pieceId - 1).endIndex && start < _pieces(pieceId).startIndex) {
      (0 until dest.size).foreach(i => dest(i) = 'N')  // if pos is between two chromosomes, it's invalid, so just return all N's
      return
    }
    val piece = _pieces(pieceId)
    val startInPiece = (start - piece.startIndex).toInt
    val endInPiece = (end - piece.startIndex).toInt
    val data = piece.data
    var i = startInPiece
    while (i < 0) {
      try {
        dest(i - startInPiece) = 'N'
      } catch {
        case e: Throwable => {
          println("i:" + i)
          println("start:" + start)
          println("end:" + end)
          e.printStackTrace
        }
      }
      i += 1
    }
    System.arraycopy(data, i, dest, i - startInPiece, min(endInPiece, data.length) - i)
    i = min(endInPiece, data.length)
    while (i < endInPiece) {
      dest(i - startInPiece) = 'N'
      i += 1
    }
  }
  
  def rcSubstring(start: Long, end: Long): String = {
    DNA.reverseComplement(substring(start, end))
  }
  
  def getLocation(pos: Long): (String, Int) = {
    var pieceId = 0
    while (pos > _pieces(pieceId).endIndex && pieceId < _pieces.size)
      pieceId += 1
    val piece = _pieces(pieceId)
    val posInPiece = (pos - piece.startIndex).toInt
    (piece.name, posInPiece)
  }
  
  def getAbsPos(pieceName: String, posInPiece: Long): Long = {
    val piece = getPiece(pieceName)
    val absPos = piece.startIndex + posInPiece
    assert(absPos <= piece.endIndex)

    absPos
  }
  
  def getPieceId(pieceName: String): Int = {
    var pieceId = 0
    (0 until pieces.length).foreach(p => {
      if (pieces(p).name == pieceName)
        pieceId = p
    })
    pieceId
  }
  
  def getPiece(pieceName: String): GenomePiece = {
    pieces(getPieceId(pieceName))
  }
  
  def getPiece(pos: Long): GenomePiece = {
    var pieceId = 0
    while (pos > _pieces(pieceId).endIndex && pieceId < _pieces.size)
      pieceId += 1
    _pieces(pieceId)
  }

  def containsPiece(pieceName: String): Boolean = {
    var found = false
    pieces.foreach(p => {
      if (p.name == pieceName)
        found = true
    })
    found
  }
  
  def addPiece(name: String, data: Array[Byte]) {
    val offset = if (_pieces.isEmpty) 0 else _pieces.last.endIndex + INDEX_GAP
    _pieces += GenomePiece(name, data, offset)
  }
  
  def pieces: IndexedSeq[GenomePiece] = _pieces
  
  def addToIndex(builder: IndexBuilder) {
    val seedLen = builder.seedLen
    for (p <- pieces) {
      println("Indexing " + p.name)
      val data = p.data
      var lastN = -1 // last index where we saw an 'N'
      var key = 0L
      val mask = (1L << (seedLen * 2)) - 1
      var i = 0
      val printInterval = 1000000
      var nextPrint = printInterval
      while (i < data.length) {
        val base = data(i)
        key = ((key << 2) | DNA.BASE_TO_CODE(base)) & mask
        if (base == 'N')
          lastN = i
        if (lastN <= i - seedLen)
          builder.add(key, i - seedLen + 1 + p.startIndex)
        if (i == nextPrint) {
          println("Position: %d/%d".format(i, data.length))
          nextPrint += printInterval
        }
        i += 1
      }
    }
  }

  /*
  def updatePiece(pieceName: String, m: MultiVCF): Genome = {
    // get new genome piece with variants from multi vcf
    // iterate in reverse so that updating indels won't mess with pos of downstream variants
    val newData = getPiece(pieceName).updateData(m.iterateInReverse(this).filter(_.piece == pieceName))
    val g = new Genome
    g.addPiece(pieceName, newData)
    g
  }
  */
  
  // assumes start & end are absolute positions
  def addToIndex(builder: IndexBuilder, start: Long, end: Long) {
    val seedLen = builder.seedLen
    
    val (startPieceName, startPos) = getLocation(start)
    val (endPieceName, endPos) = getLocation(end)
    
    val startPieceId = getPieceId(startPieceName)
    val endPieceId = getPieceId(endPieceName)
    
    var p = startPieceId
    while (p <= endPieceId) {
      println("Indexing " + pieces(p).name)
      val data = pieces(p).data
      var lastN = -1 // last index where we saw an 'N'
      var key = 0L
      val mask = (1L << (seedLen * 2)) - 1
      var i = 
        if (p == startPieceId)
          startPos.toInt
        else
          0
      val printInterval = 1000000
      var nextPrint = max(printInterval, i)
      
      val endPosInPiece = 
        if (p == endPieceId)
          endPos
        else
          data.length - 1
      
      //while (i < data.length && i <= endPos) {
      while (i <= endPosInPiece) {
        val base = data(i)
        key = ((key << 2) | DNA.BASE_TO_CODE(base)) & mask
        if (base == 'N')
          lastN = i
        if (lastN <= i - seedLen)
          builder.add(key, i - seedLen + 1 + pieces(p).startIndex)
        if (i == nextPrint) {
          println("Position: %d/%d".format(i, data.length))
          nextPrint += printInterval
        }
        i += 1
      }
            
      p += 1
    }
  }
  
  /*
  def addToIndexAsString(builder: BucketIndexBuilder) {
    val seedLen = builder.seedLen
    for (p <- pieces) {
      println("Indexing " + p.name)
      val data = p.data
      var lastN = -1  // last index where we saw an 'N'
      var i = 0
      val printInterval = 1000000
      var nextPrint = printInterval
      
      val iVal = 18000000

      while (i < data.length) {
        var foundN = false
        var seed = new StringBuilder(seedLen)
        seed.insertAll(0, Array.fill(seedLen)(' '))
        
        breakable {
          (0 until seedLen).foreach(j => {
            val base = data(i + j)
            if (base == 'N') {
              foundN = true
              lastN = i + j
              break()
            } else
              seed(j) = base.toChar
          })
        }

        // yes => start over after that N
        // no => add these seedLen bases to the index
        if (foundN) {
          i = lastN + 1
        } else {
	        val seedStr = seed.toString
	        if (seedStr.length == seedLen)
	          builder.add(seedStr, p.startIndex + i)
          i += 1
        }

        if (i == nextPrint) {
          println("Position: %d/%d".format(i, data.length))
          nextPrint += printInterval
          //println("Current Seed: " + seed)
        }
      }
    }
  }
  */
  
  var currentPiece = 0
  var nextReadStart = 0
  
  def getNextRead(readLen: Int): String = {
    val piecesArray = pieces
    val p = piecesArray(currentPiece)
    val data = p.data
    var i = nextReadStart
    var lastN = 0
    
    while (i < data.length) {
      var foundN = false
      var read = new StringBuilder(readLen)
      read.insertAll(0, Array.fill(readLen)(' '))
      
      breakable {
        (0 until readLen).foreach(j => {
          val base = data(i + j)
          if (base == 'N') {
            foundN = true
            lastN = i + j
            break()
          } else
            read(j) = base.toChar
        })
      }

      // encountered an N?
      // yes => start over after that N
      // no => return these readLen bases
      if (foundN) {
        i = lastN + 1
      } else {
        val readStr = read.toString
        if (readStr.length == readLen) {
          nextReadStart = i + 1
          return readStr
        }
      }
    }
    
    currentPiece += 1
    "notFound"
  }
  
  /*
  // would supercede all these functions (add to index as string, get read list, get read list & pos)
  def scanReads(readLen: Int, f: ((String, Long) => Unit)): Unit = {
    // for each read in genome
    // call function on read & its pos
    
    for (p <- pieces) {
      println("Scanning reads from " + p.name + "...")
      val data = p.data
      var lastN = -1
      var i = 0
      val printInterval = 1000000
      
      while (i < data.length) {
        var foundN = false
        var read = new StringBuilder(readLen)
        read.insertAll(0, Array.fill(readLen)(' '))

        try {
          breakable {
            (0 until readLen).foreach(j => {
              if (i + j < data.length) {
                val base = data(i + j)
                if (base == 'N') {
                  foundN = true
                  lastN = i + j
                  break()
                } else
                  read(j) = base.toChar
              }
            })
          }

          // encountered an N?
          // yes => start over after that N
          // no => call f on these bases & the initial pos
          if (foundN) {
            i = lastN + 1
          } else {
            val readStr = read.toString
            if (readStr.length == readLen) {
              f(readStr, p.startIndex + i)
            }
            i += 1
          }

          if (i % printInterval == 0)
            println("Position: %d/%d".format(i, data.length))
        } catch {
          case e => {
            println(i)
            println(read.toString + " at " + (p.startIndex + i))
            e.printStackTrace
          }
        }
      }
    }
  }
  */
  
  def scanReads(readLen: Int, f: (Long => Unit)): Unit = {
    for (p <- pieces) {
      println("Indexing " + p.name)
      val data = p.data
      var lastN = -1 // last index where we saw an 'N'
      var key = 0L
      var i = 0
      val printInterval = 1000000
      var nextPrint = printInterval
      while (i < data.length) {
        val base = data(i)
        if (base == 'N')
          lastN = i
        if (lastN <= i - readLen)
          f(i - readLen + 1 + p.startIndex)
        if (i == nextPrint) {
          println("Position: %d/%d".format(i, data.length))
          nextPrint += printInterval
        }
        i += 1
      }
    }
  }
  
  
  def getReadList(readLen: Int): List[String] = {
    import scala.collection.mutable.ListBuffer
    
    val readListBuf = new ListBuffer[String]

    for (p <- pieces) {
      println("Getting reads from " + p.name + "...")
      val data = p.data
      var lastN = -1  // last index where we saw an 'N'
      var i = 0
      val printInterval = 1000000
      var nextPrint = printInterval
      
      while (i < data.length) {
        var foundN = false
        var read = new StringBuilder(readLen)
        read.insertAll(0, Array.fill(readLen)(' '))
        
        breakable {
          (0 until readLen).foreach(j => {
            val base = data(i + j)
            if (base == 'N') {
              foundN = true
              lastN = i + j
              break()
            } else
              read(j) = base.toChar
          })
        }

        // encountered an N?
        // yes => start over after that N
        // no => add these readLen bases to the index
        if (foundN) {
          i = lastN + 1
        } else {
	        val readStr = read.toString
	        if (readStr.length == readLen)
	          readListBuf += readStr
          i += 1
        }

        if (i == nextPrint) {
          println("Position: %d/%d".format(i, data.length))
          nextPrint += printInterval
        }
      }
    }
    
    readListBuf.toList
  }
  
  def totalSize: Long = pieces.map(_.data.length.toLong).sum
  
  def nonNSize: Long = pieces.map(_.data.filter(_ != 'N').length.toLong).sum
}
