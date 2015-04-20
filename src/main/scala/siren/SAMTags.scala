package snap

case class SAMTag(tagName: String, tagType: String, value: String) {
  override def toString = tagName + ":" + tagType + ":" + value
}

object SAMTag {
  val TAG_REGEX = """([A-Za-z][A-Za-z0-9]):(.+):(.+)""".r

  def getSAMTag(t: String): SAMTag = {
    val TAG_REGEX(tagName, tagType, value) = t
    SAMTag(tagName, tagType, value)
  }
}

class SAMTags(tags: List[String]) {
  val tagsMap = scala.collection.mutable.Map[String, SAMTag]()
  tags.foreach(t => {
    val samTag = SAMTag.getSAMTag(t)
    tagsMap += ((samTag.tagName, samTag))
  })
  
  def getTag(tagName: String): SAMTag = {
    tagsMap.get(tagName) match {
      case Some(tag) => {
        tag
      } case None => {
        println("Warning:  tag " + tagName + " not found.")
        null
      }
    }
  }
  
  def addTag(tag: SAMTag) {
    tagsMap.get(tag.tagName) match {
      case Some(samTag) => {
        println("Warning:  tag " + samTag.toString + " already exists, and you've tried to add it.")
      } case None => {
        tagsMap += ((tag.tagName, tag))
      }
    }
  }
  
  def addTag(t: String) {
    addTag(SAMTag.getSAMTag(t))
  }

  def addOrOverwriteTag(tag: SAMTag) {
    tagsMap.get(tag.tagName) match {
      case Some(samTag) => tagsMap.remove(tag.tagName)
      case None => println("Warning: tag " + tag.toString + " does not exist, and you've tried to overwrite it.")
    }
    addTag(tag)
  }
  
  // the only thing that COULD be a problem here is the order... hopefully it's ok in random order.
  // if not, I'll have to figure out how to sort them
  override def toString = {
    var s = ""
    tagsMap.keys.foreach(k => {
      if (s != "")
        s += "\t"
      val t = tagsMap.get(k).get
      s += t.toString
    })
    s
  }
}