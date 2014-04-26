package org.ljudmila.liminoid.hardware

import de.fruitfly.ovr.OculusRift

case class Rotation(yaw: Float, pitch: Float, roll: Float) {
  def +(r: Rotation): Rotation = Rotation(yaw+r.yaw, pitch+r.pitch, roll+r.roll)
  def -(r: Rotation): Rotation = Rotation(yaw-r.yaw, pitch-r.pitch, roll-r.roll)
  def *(f: Float): Rotation = Rotation(yaw*f, pitch*f, roll*f)
}

object RiftTracker {
  def init(): Unit = { } //Foo
  var initialized = true
  def withInit[T](f: => T): Option[T] = {
    var out: Option[T] = None
    initialized = initialized && (try { out = Option(f); true } catch { case _: Throwable => false })
    out
  }
  withInit { OculusRift.LoadLibrary(new java.io.File(System.getProperty("java.io.tmpdir"))); }

  val rift = withInit { 
    val r = new OculusRift
    r.init
    r
  } getOrElse null
  
  
  def poll(): Rotation = withInit {
    rift.poll()
    
    Rotation(
      rift.getYawDegrees_LH(),
      rift.getPitchDegrees_LH(),
      rift.getRollDegrees_LH())
  } getOrElse Rotation(0,0,0)
  
  def destroy(): Unit = withInit { rift.destroy }
}
