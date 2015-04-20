package snap
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.io.BufferedOutputStream
import java.io.FileOutputStream

object FASTQ {
  def read(file: String, keepQuality: Boolean = true): IndexedSeq[Read] = {
    val lines = Source.fromFile(file).getLines
    val reads = new ArrayBuffer[Read]
    for (Seq(id, data, plus, quality) <- lines.filter(_ != "").sliding(4, 4)) {
      //reads += new Read(id.substring(1).getBytes, data.getBytes, 
      reads += new Read(id.getBytes, data.getBytes, 
        if (keepQuality) quality.getBytes else null)
    }
    reads
  }
  
  def writer(file: String) = new Writer(file)
  
  class Writer(file: String) {
    private val out = new BufferedOutputStream(new FileOutputStream(file))
    
    def write(read: Read): Unit = {
      if (read.quality == null)
        throw new IllegalArgumentException("Read has no quality information")
      out.write('@')
      out.write(read.id)
      out.write('\n')
      out.write(read.data)
      out.write('\n')
      out.write('+')
      out.write('\n')
      out.write(read.quality)
      out.write('\n')
    }
    
    def close() {
      out.close()
    }
  }
}
