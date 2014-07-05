package org.ljudmila.liminoid

import java.io.FileInputStream
import java.io.BufferedInputStream
import javazoom.jl.player.Player
import Utils.{ thread, getFile }

object Sound {
  val folder = "snd/"
  private var muted = false
  def mute(): Unit = synchronized {
    stopAll()
    muted = true
  }
  def unmute(): Unit = synchronized {
    muted = false
  }
  val soundMap = getFile(folder + "list.txt").map { line =>
    val name :: file :: _ = line.split(" ").toList
    (name, folder + file)
  }.toMap
  
  def init(): Unit = synchronized {
    for((_, file) <- soundMap) {
      val player = new Player(new BufferedInputStream(new FileInputStream(file)))
      player.play(0)
      player.close()
    }
  }
  
  var players = Set.empty[Player]
  def stopAll(): Unit = synchronized {
    players.foreach(_.close)
    players = Set.empty
  }

  def play(sound: String): Unit = {
    if(!muted) {
      thread {
        val player = new Player(new BufferedInputStream(new FileInputStream(soundMap(sound))))
        synchronized {
          players += player
        }

        player.play

        synchronized {
          players -= player
          player.close
        }
      }
    }
  }
}
