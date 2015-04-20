package siren

class Read(val id: Array[Byte], val data: Array[Byte], val quality: Array[Byte]) extends Serializable {
  def idStr = new String(id)
  def dataStr = new String(data)
  def qualityStr = new String(quality)
  
  def ==(that: Read): Boolean = {
    idStr == that.idStr && dataStr == that.dataStr && qualityStr == that.qualityStr
  }
}
