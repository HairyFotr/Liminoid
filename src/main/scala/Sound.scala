package org.ljudmila.liminoid

import java.io.FileInputStream
import java.io.BufferedInputStream
import javazoom.jl.player.Player
import Utils.{thread, getFile}

object Sound {
  val folder = "snd/"
  val soundMap = getFile(folder + "list.txt") map { line => 
    val name :: file :: _ = line.split(" ").toList
    (name, folder + file)
  } toMap
  
  def init(): Unit = {
    for((_, file) <- soundMap) {
      val player = new Player(new BufferedInputStream(new FileInputStream(file)))
      player.play(0)
      player.close()
    }
  }
  
  def play(sound: String): Unit = thread {
    val player = new Player(new BufferedInputStream(new FileInputStream(soundMap(sound))))
    player.play
    player.close
  }
}
