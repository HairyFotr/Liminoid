package org.ljudmila.liminoid
import java.io._

sealed trait Sequence[T] {
  def path: String
  def ext: String
  def delay: Int
  def bounce: Boolean
  var active: Boolean
  def stopAtEnd: Boolean

  //---//

  var direction = 1
  var cursor = 0
  val files = 
    (new File(path)).listFiles
    .filter { _.isFile }
    .filter { _.toString endsWith ext }

  val frames = files.map { _.toString }.sorted

  private var time = 0

  def preload(): Unit
  def get(): T
  def apply(): T = 
    if(active) {
      val out = get()
      moveCursor()
      out
    } else {
      get()
    }

  def rewind(): Unit = {
    cursor = 0
    direction = 1
  }

  def moveCursor(): Unit = {
    val currCursor = cursor
    if(Utils.now - time >= delay) {
      cursor = (cursor + direction) % frames.size
      if(currCursor == frames.size - 1 && stopAtEnd) {
        cursor = frames.size - 1
        active = false
      } else if(bounce) {
        if(currCursor == frames.size - 1 && cursor == 0) {
          cursor = frames.size - 2
          direction = -1
        } else if(currCursor == 0 && cursor == -1) {
          cursor = 1
          direction = 1
        }
      }
      time = Utils.now
    }
  }
}

case class TexSequence(
    val path: String, 
    var active: Boolean = true,
    var delay: Int = 175,
    var bounce: Boolean = true,
    var stopAtEnd: Boolean = false,
    val ext: String = ".png") extends Sequence[Int] { 
  override def get() = Texture(frames(cursor))
  override def preload() { Texture.preload(files) }
}

import Model._
case class OBJSequence(
    val path: String,
    var transform: Model.Transform = Model.Transform001,
    var transformVectors: Model.Transform = Model.Transform000,
    var active: Boolean = true,
    var delay: Int = 75,
    var bounce: Boolean = true,
    var stopAtEnd: Boolean = false,
    val ext: String = ".obj") extends Sequence[Model] { 
  override def get() = OBJModel(frames(cursor)).toModel(transform = transform)
  override def preload() { OBJModel.preload(files) }
}














