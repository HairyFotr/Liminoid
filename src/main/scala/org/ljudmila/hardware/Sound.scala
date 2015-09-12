package org.ljudmila.hardware

import java.io.FileInputStream
import java.io.BufferedInputStream
import javazoom.jl.player.Player
import org.ljudmila.Utils.{ thread, getFile }
import scala.collection.mutable

final object Sound {
  private var muted = false
  def mute(): Unit = synchronized {
    stopAll()
    muted = true
  }
  def unmute(): Unit = synchronized {
    muted = false
  }
  val soundMap = mutable.HashMap.empty[String, String]
  
  def init(folder: String): Unit = synchronized {
    for (line <- getFile(folder + "list.txt")) {
      val (name, file) = line.splitAt(line.indexOf(' '))
      soundMap += name -> (folder + file.trim)
    }

    for ((_, file) <- soundMap) {
      val player = new Player(new BufferedInputStream(new FileInputStream(file)))
      player.play(0)
      player.close()
    }
  }
  
  var players = Set.empty[Player]
  def stopAll(): Unit = players.synchronized {
    players.foreach(_.close)
    players = Set.empty
  }

  def play(sound: String): Unit = {
    if (!muted) {
       if (players.size <= 12) thread {
        val player = new Player(new BufferedInputStream(new FileInputStream(soundMap(sound))))
        players.synchronized {
          players += player
        }

        player.play

        players.synchronized {
          players -= player
          player.close
        }
      } else {
        println("Too much sounds, dropping: "+sound)
      }
    }
  }
}
