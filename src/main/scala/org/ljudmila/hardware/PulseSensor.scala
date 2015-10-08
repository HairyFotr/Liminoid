package org.ljudmila.hardware

import jssc._
import jssc.SerialPort.BAUDRATE_115200
import jssc.SerialPort.DATABITS_8
import jssc.SerialPort.MASK_RXCHAR
import jssc.SerialPort.PARITY_NONE
import jssc.SerialPort.STOPBITS_1
import org.ljudmila.Utils.{ now, since }
import org.ljudmila.SettingsReader

object PulseSensor {
  import jssc.SerialPort._

  /*def main(args: Array[String]): Unit = {
    try {
      PulseSensor.init()
      import Sound._
      val settings = SettingsReader.load("Settings.txt")
      Sound.init(settings("snds"))
      if (!PulseSensor.init_) {
        println("Using fake pulse")
        PulseSensor.fake = true
        PulseSensor.init_ = true
      }
      while (true) {
        val a = PulseSensor.takeBeat();
        if (a) {
          Sound.play("heartbeep")
          println("beep")
        }
        Thread.sleep(10+util.Random.nextInt(20))
        //print((if (PulseSensor.takeBeat()) "ooOOOOoo" else "--------") + "\r")
      }
      
    } finally {
      PulseSensor.close()
    }
  }*/

  var port: SerialPort = null
  var init_ = false
  def init(): Unit = {
    if (!init_) try {
      try {
        port = new SerialPort("/dev/ttyACM0")
      } catch {
        case e: Exception =>
          try {
            port = new SerialPort("/dev/ttyACM1")
          } catch {
            case e: Exception => throw e;
          }
          
      }
      port.openPort
      port.setParams(BAUDRATE_115200, DATABITS_8, STOPBITS_1, PARITY_NONE)
      port.addEventListener(new SerialPortReader, MASK_RXCHAR)
      init_ = true
    } catch {
      case e: Exception =>
         e.printStackTrace();
         fake = true;
    }
  }

  def close(): Unit = if (init_) port.closePort

  var beat = false
  var skipBeat = false
  var beatTimer = 0
  var fake = false
  val xxx = 1 // synch placeholder

  def takeBeat(): Boolean =
    if (fake) {
      if (since(beatTimer) > 750) {
        beatTimer = now
        
        true
      } else {
        false
      }
    } else synchronized {
      if (beat) {
        beat = false
        true
      } else {
        false
      }
    }

  val reg = "[|]([0-9.]+)[|]".r
  class SerialPortReader extends SerialPortEventListener {
    override def serialEvent(event: SerialPortEvent): Unit = {
      if (event.isRXCHAR()) {// If data is available
        val data = port.readString()
        if (!reg.findAllIn(data.trim()).isEmpty) synchronized {
          beat = true;
        }
      }
    }
  }
}
