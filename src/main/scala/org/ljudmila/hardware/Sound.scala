package org.ljudmila.hardware

import org.ljudmila.Utils.{ thread, getFile }
import scala.collection.mutable

// http://memeorama.com/wp-content/uploads/2012/02/call-the-cops-i-dont-give-a-fuck-llama.jpg
final object Sound {
  private[this] var muted = false
  def mute(): Unit = synchronized {
    stopAll()
    muted = true
  }
  def unmute(): Unit = synchronized {
    muted = false
  }
  val soundMap = mutable.HashMap.empty[String, String]
  
  def init(folder: String): Unit = {
    for(line <- getFile(folder + "list.txt")) {
      val (name, file) = line.splitAt(line.indexOf(' '))
      soundMap += name -> (folder + file.trim)
    }
  }
  
  def stopAll(): Unit = {
      import sys.process._
      Seq("pkill", "-9", "play").!
  }

  def play(sound: String): Unit = {
    if(!muted) thread {
      import sys.process._
      Seq("play", "-q", soundMap(sound)).!
    }
  }
}
