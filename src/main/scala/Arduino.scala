package org.ljudmila.liminoid.hardware

import java.io.{BufferedReader, InputStreamReader, OutputStream}
import gnu.io.{CommPortIdentifier, SerialPort, SerialPortEvent, SerialPortEventListener}
// Adapted from http://playground.arduino.cc/Interfacing/Java via http://javatoscala.com/

object Arduino {
  private val PORT_NAMES = Array("/dev/tty.usbserial-A9007UX1", "/dev/ttyUSB0", "COM3")
  private val TIME_OUT = 2000
  private val DATA_RATE = 9600

  /*def main(args: Array[String]) {
    val main = new SerialTest()
    main.initialize()
    //"In my experience, it seems that the Serial connection must be connected somewhere above 1 second (I have 1.5 seconds in my program) before a transmission can be made."
    Thread.sleep(1500)
    val t = new Thread() {
      override def run() {
        try {
          Thread.sleep(1000000)
        } catch {
          case ie: InterruptedException => 
        }
      }
    }
    t.start()
    println("Started")
  }*/

  class SerialTest extends SerialPortEventListener {
    var serialPort: SerialPort = _
    private var input: BufferedReader = _
    private var output: OutputStream = _

    def initialize() {
      var portId: CommPortIdentifier = null
      val portEnum = CommPortIdentifier.getPortIdentifiers
      while (portEnum.hasMoreElements()) {
        val currPortId = portEnum.nextElement().asInstanceOf[CommPortIdentifier]
        for (_ <- PORT_NAMES find { portName => portName == currPortId.getName }) {
          portId = currPortId
          //break
        }
      }
      if (portId == null) {
        println("Could not find COM port.")
        return
      }
      try {
        serialPort = portId.open(this.getClass.getName, TIME_OUT).asInstanceOf[SerialPort]
        serialPort.setSerialPortParams(DATA_RATE, SerialPort.DATABITS_8, SerialPort.STOPBITS_1, SerialPort.PARITY_NONE)
        input = new BufferedReader(new InputStreamReader(serialPort.getInputStream))
        output = serialPort.getOutputStream
        serialPort.addEventListener(this)
        serialPort.notifyOnDataAvailable(true)
      } catch {
        case e: Exception => System.err.println(e.toString)
      }
    }

    def close() {
      synchronized {
        if (serialPort != null) {
          serialPort.removeEventListener()
          serialPort.close()
        }
      }
    }

    def serialEvent(oEvent: SerialPortEvent) {
      synchronized {
        if (oEvent.getEventType == SerialPortEvent.DATA_AVAILABLE) {
          try {
            val inputLine = input.readLine()
            println(inputLine)
          } catch {
            case e: Exception => System.err.println(e.toString)
          }
        }
      }
    }
  }
}
