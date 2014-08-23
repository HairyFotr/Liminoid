package org.ljudmila.liminoid.hardware

import jssc._
import jssc.SerialPort.BAUDRATE_115200
import jssc.SerialPort.DATABITS_8
import jssc.SerialPort.MASK_RXCHAR
import jssc.SerialPort.PARITY_NONE
import jssc.SerialPort.STOPBITS_1
import org.ljudmila.liminoid.Utils.{ now, since }

object PulseSensor {
  import jssc.SerialPort._

  var port: SerialPort = null
  var init_ = false
  def init(): Unit = {
    if(!init_) try {
      port = new SerialPort("/dev/ttyACM0")
      port.openPort
      port.setParams(BAUDRATE_115200, DATABITS_8, STOPBITS_1, PARITY_NONE)
      port.addEventListener(new SerialPortReader, MASK_RXCHAR)
      init_ = true
    } catch {
      case e: Exception =>
    }
  }

  def close(): Unit = if(init_) port.closePort

  /* //Untested code!
  var lastS, lastB, lastQ = 0
  class SerialPortReader extends SerialPortEventListener {
    override def serialEvent(event: SerialPortEvent) {
      if(event.isRXCHAR()) { // If data is available
        val data = port.readString()
        if(data.endsWith("\n")) synchronized {
          val sReg = "S([0-9]+)"
          val bReg = "B([0-9]+)"
          val qReg = "Q([0-9]+)"
          data.split("\n").foreach {
            case sReg(n) => lastS = n.toInt
            case sReg(n) => lastB = n.toInt
            case sReg(n) => lastQ = n.toInt
          }
        }
      }
    }
  }*/

  var beat = false

  var lastBeat = now-1000
  var avgBeat = 1000d // rolling average beat
  var lastBeatAlien = false // was last beat alien

  // just beat it... just beat it
  var fakeTimer = 0
  var fake = false

  def takeBeat(): Boolean =
    if(fake) {
      if(since(fakeTimer) > 900) {
        fakeTimer = now
        
        true
      } else {
        false
      }
    } else synchronized {
      val sinceLastBeat = since(lastBeat)

      var out = beat
      beat = false
      
      // Ignore alien heartbeats (sorry aliens)
      if(lastBeatAlien) {
        lastBeatAlien = false
      } else {
        avgBeat = avgBeat*0.99 + sinceLastBeat*0.01
        if(avgBeat < 400) avgBeat = 400
        if(avgBeat < 1300) avgBeat = 1300
      }

      if(sinceLastBeat < avgBeat*0.65) {
        out = false
        lastBeatAlien = true
      } else if(sinceLastBeat > avgBeat * 2) {
        out = true
        lastBeatAlien = true
      }

      if(out) lastBeat = now

      //println((out, lastBeatAlien, avgBeat))

      out
    }

  class SerialPortReader extends SerialPortEventListener {
    override def serialEvent(event: SerialPortEvent): Unit = {
      if(event.isRXCHAR()) { // If data is available
        val data = port.readString()
        if(data contains "B") synchronized { beat = true }
      }
    }
  }
}
