package org.ljudmila.liminoid
import java.io._
import Utils._

sealed trait Sequence[T] {
  def path: String
  def ext: String
  def delay: Double
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

  private var time = -1 // for simple animation where timing doesn't matter that much

  def preload(): Unit
  def get(): T
  def apply(): T = 
    (if(active) {
      val out = get()
      moveCursor()
      out
    } else {
      get()
    })

  def delete(d: String): Unit

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
    var delay: Double = 1000/24d, //24fps
    var bounce: Boolean = false,
    var stopAtEnd: Boolean = false,
    var selfDestruct: Boolean = false,
    val ext: String = ".png") extends Sequence[Int] { 

  private var startTime = -1 //for playing the whole thing through
  override def moveCursor(): Unit = { //breaks bouncing and things like that
    if(startTime == -1) startTime = now
    while(since(startTime) > cursor * delay ) cursor += 1
    if(cursor >= frames.size) {
      cursor = frames.size - 1
      active = false
    }
  }

  var last = ""
  override def get() = {
    val name = frames(cursor)
    val out = Texture(name)
    if(selfDestruct && name != last) {
      if(last.nonEmpty) delete(last)
      last = name
    }
    out
  }
  override def preload() { Texture.preload(files) }
  def preload(i: Int) { Texture.preload(files, i) }
  override def delete(d: String) { Texture.delete(d) }
}

import Model._
case class OBJSequence(
    val path: String,
    val transform: MutableTransform = Transform001,
    val transformVector: MutableTransform = Transform000,
    var oscillatorPhase: Double = 0,
    var active: Boolean = true,
    var delay: Double = 75,
    var coreTransform: MutableTransform,
    var bounce: Boolean = true,
    var stopAtEnd: Boolean = false,
    val ext: String = ".obj") extends Sequence[Model] { 

  val models: Array[Model] = frames.map { name => OBJModel(name).toModel(transform = transform, color = Color(0.85), coreTransform = coreTransform) }
  override def get() = models(cursor)
  override def preload() { OBJModel.preload(files) }
  def preload(i: Int) { OBJModel.preload(files, i) }
  override def delete(d: String) { /*foo*/ }
}














