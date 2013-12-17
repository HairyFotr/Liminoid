package org.ljudmila.liminoid.hardware

import java.io.{BufferedReader, InputStreamReader, OutputStream}
import jssc._;

object PulseSensor {
  import jssc.SerialPort._

  var port: SerialPort = null
  var init_ = false
  def init() {
    if(!init_) try {
      port = new SerialPort("/dev/ttyACM0")
      port.openPort()
      port.setParams(BAUDRATE_115200, DATABITS_8, STOPBITS_1, PARITY_NONE)
      port.addEventListener(new SerialPortReader(), MASK_RXCHAR)
      init_ = true
    } catch {
      case e: Exception =>        
    }
  }

  def close() {
    if(init_) { port.closePort() }
  }

  /* //Untested code!
  var lastS,lastB,lastQ = 0
  class SerialPortReader extends SerialPortEventListener {
    override def serialEvent(event: SerialPortEvent) {
      if(event.isRXCHAR()) {//If data is available
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

  import org.ljudmila.liminoid.Utils.{now, since}

  var lastBeat = now-1000
  var avgBeat = 1000d //rolling average beat
  var lastBeatAlien = false //was last beat alien

  def takeBeat(): Boolean = synchronized {
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
    override def serialEvent(event: SerialPortEvent) {
      if(event.isRXCHAR()) {//If data is available
        val data = port.readString()
        if(data contains "B") synchronized { beat = true }
      }
    }
  }
}