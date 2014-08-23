package org.ljudmila.liminoid.hardware

import de.fruitfly.ovr.OculusRift
import java.io.File
import org.ljudmila.liminoid.Model.{ Rotation, rotation0 }

object RiftTracker {
  def init(): Unit = { } //Foo
  var initialized = true
  def withInit[T](f: => T): Option[T] = {
    var out: Option[T] = None
    initialized = initialized && (try { out = Option(f); true } catch { case _: Throwable => false })
    out
  }
  
  withInit { OculusRift.LoadLibrary(new File(System.getProperty("java.io.tmpdir"))); }

  private[this] val oculusRift = withInit {
    val or = new OculusRift
    or.init
    or
  } getOrElse null
  
  
  def poll(): Rotation = withInit {
    oculusRift.poll()
    
    Rotation(
      oculusRift.getYawDegrees_LH(),
      oculusRift.getPitchDegrees_LH(),
      oculusRift.getRollDegrees_LH())
  } getOrElse rotation0
  
  def destroy(): Unit = withInit { oculusRift.destroy }
}
