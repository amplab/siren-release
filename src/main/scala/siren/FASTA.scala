package snap

import scala.io.Source
import java.io.BufferedOutputStream
import java.io.FileOutputStream

object FASTA {
  def read(file: String): Genome = {
    var curName: String = null
    var curData = new ByteArrayBuilder
    val genome = new Genome
    for (line <- Source.fromFile(file).getLines) {
      if (line.startsWith(">")) {
        if (curName != null) {
          genome.addPiece(curName, curData.toArray)
          curData.clear()
        }
        curName = line.substring(1)
        //curData.append('N'.toByte) // To make pieces 1-indexed  /* for now, want it to be 0-indexed so it's compatible with C++ version of SNAP */
      } else {
        curData.append(line.toUpperCase.getBytes)
      }
    }
    if (curName != null) {
      genome.addPiece(curName, curData.toArray)
      curData.clear()
    }
    genome
  }
  
  def writer(file: String) = new Writer(file)
  
  class Writer(file: String) {
    private val out = new BufferedOutputStream(new FileOutputStream(file))
    
    def write(genome: Genome): Unit = {
      val columnWidth = 50

      genome.pieces.foreach(p => {
        out.write('>')
        out.write(p.name.getBytes)
        out.write('\n')
        
        (0 until p.data.length).foreach(i => {
          out.write(p.data(i))
          if (i > 0 && i % columnWidth == (columnWidth - 1))
            out.write('\n')
        })
        out.write('\n')
      })
    }
    
    def close() {
      out.close()
    }
  }
}
