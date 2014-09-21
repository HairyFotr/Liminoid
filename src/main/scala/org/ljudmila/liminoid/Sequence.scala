package org.ljudmila.liminoid
import java.io._

import Models._
import org.ljudmila.Utils._
import org.ljudmila.hardware.Camera

sealed trait Sequence[T] {
  def path: String
  def ext: String
  def delay: Double
  def bounce: Boolean
  var active: Boolean
  def stopAtEnd: Boolean

  //---//
  println(path)

  var direction = 1
  var cursor = 0
  var startTime = -1
  val files =
    (new File(path))
      .listFiles
      .filter { _.isFile }
      .filter { _.toString endsWith ext }

  val frames = files.map { _.toString }.sorted

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

  def reset(): Unit = rewind
  def rewind(): Unit = {
    cursor = 0
    direction = 1
    startTime = -1
  }

  def moveCursor(): Unit = synchronized {
    if(startTime == -1) startTime = now
    val lastFrame = frames.size-1

    //if(cursor < 0 || cursor > framesm1) new Exception("Wat: " + cursor)
    if(direction > 0) {
      //TODO: just calculate it... also, cursor + 1 or first frame gets skipped?
      while(since(startTime) > cursor * delay) cursor += 1
      if(cursor >= lastFrame) {
        val diff = cursor - lastFrame
        if(bounce) {
          cursor = math.max(0, lastFrame - diff) //TODO: Possible multiple bounces, but meh
          //if(cursor < 0 || cursor > framesm1) new Exception("Wat: " + cursor)
          direction = -direction
          startTime = now //TODO: not right
        } else { // if(loop)
          cursor = math.min(diff, lastFrame)
          //if(cursor < 0 || cursor > framesm1) new Exception("Wat: " + cursor)
        }
        
        if(stopAtEnd) {
          cursor = lastFrame
          //if(cursor < 0 || cursor > framesm1) new Exception("Wat: " + cursor)
          active = false
        }
      }
    } else if(direction < 0) {
      while(since(startTime) > (lastFrame - cursor) * delay) cursor -= 1
      if(cursor <= 0) {
        if(bounce) {
          cursor = math.min(-cursor, lastFrame)
          //if(cursor < 0 || cursor > framesm1) new Exception("Wat: " + cursor)
          direction = -direction
          startTime = now
        } else { // if(loop)
          cursor = math.max(lastFrame + cursor, 0)
          //if(cursor < 0 || cursor > framesm1) new Exception("Wat: " + cursor)
        }
        
        if(stopAtEnd) {
          cursor = 0
          //if(cursor < 0 || cursor > framesm1) new Exception("Wat: " + cursor)
          active = false
        }
      }
    }
  }
}

/*sealed trait VideoSource {
  def get(): Int
}
case class ImageFolder(path: String) extends VideoSource {
  
}
case class Camera(cam: Camera) extends VideoSource {
  
}*/

case class TexSequence(
    val path: String,
    var active: Boolean = true,
    var delay: Double = 1000/24d, //24fps
    var bounce: Boolean = false,
    var stopAtEnd: Boolean = false,
    var selfDestruct: Boolean = false,
    val ext: String = ".png") extends Sequence[Int] {

  var prev = ""
  override def get(): Int = {
    val name = frames(cursor)
    val out = Texture(name)
    if(selfDestruct && name != prev) {
      if(prev.nonEmpty) delete(prev)
      prev = name
    }
    out
  }
  override def preload(): Unit = { Texture.preload(files) }
  def preload(i: Int): Unit = { Texture.preload(files, i) }
  override def delete(d: String): Unit = { Texture.delete(d) }
  def clear(): Unit = { for(f <- frames) delete(f) }
}
case class OBJSequence(
    val path: String,
    val transform: MutableTransform = transform001,
    val transformVector: MutableTransform = transform000,
    var oscillatorPhase: Double = 0,
    var active: Boolean = true,
    var delay: Double = 80,
    var color: Color,
    var coreTransform: MutableTransform,
    var bounce: Boolean = true,
    var stopAtEnd: Boolean = false,
    val ext: String = ".obj") extends Sequence[Model] {

  val models: Array[Model] = frames.map { name => OBJModel.load(name).toModel(transform = transform, color = color, coreTransform = coreTransform) }
  override def get(): Model = models(cursor)
  override def preload(): Unit = { OBJModel.preload(files) }
  def preload(i: Int): Unit = { OBJModel.preload(files, i) }
  override def delete(d: String): Unit = { /*foo*/ }
}
