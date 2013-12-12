package org.ljudmila.liminoid

import java.io.FileInputStream
import java.io.BufferedInputStream
import javazoom.jl.player.Player
import Utils.{thread, getFile}
import scala.collection.mutable.{Map, SynchronizedMap}
object Sound {
  val folder = "snd/"
  var mute = true//todo def mute close all, prevent new
  val soundMap = getFile(folder + "list.txt").map { line => 
    val name :: file :: _ = line.split(" ").toList
    (name, folder + file)
  }.toMap
  
  def init(): Unit = {
    for((_, file) <- soundMap) {
      val player = new Player(new BufferedInputStream(new FileInputStream(file)))
      player.play(0)
      player.close()
    }
  }
  
  var players = Set.empty[Player]
  def stopAll() = this.synchronized {
    players.foreach(_.close)
    players = Set.empty
  }

  def play(sound: String): Unit = thread {
    val player = new Player(new BufferedInputStream(new FileInputStream(soundMap(sound))))
    this.synchronized { 
      players += player
    }

    player.play

    this.synchronized {
      players -= player
      player.close
    }
  }
}
