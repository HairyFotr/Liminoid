package org.ljudmila.liminoid
import java.io._

sealed trait Sequence[T] {
  def path: String
  def ext: String
  def delay: Int
  def bounce: Boolean
  def active: Boolean

  //---//

  var direction = 1
  var cursor = 0
  val frames = 
    (new File(path)).listFiles
    .map { _.toString }
    .filter { _ endsWith ext }
    .toIndexedSeq
    .sorted

  private var time = 0

  def get(): T
  def apply(): T = 
    if(active) {
      val out = get()
      moveCursor()
      out
    } else {
      get()
    }

  def moveCursor(): Unit = {
    val currCursor = cursor
    if(Utils.now - time > delay) {
      cursor = (cursor + direction) % frames.size
      if(bounce) {
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
    val ext: String = ".png") extends Sequence[Int] { 
  override def get() = Texture(frames(cursor))
}

case class OBJSequence(
    val path: String,
    var pos: OBJModel.Vec = OBJModel.Vec0,
    var rot: OBJModel.Vec = OBJModel.Vec0,
    var size: OBJModel.Vec = OBJModel.Vec1,
    var active: Boolean = true,
    var delay: Int = 15,
    var bounce: Boolean = true,
    val ext: String = ".obj") extends Sequence[OBJModel.Model] { 
  override def get() = OBJModel(frames(cursor))
}
















