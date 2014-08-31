package org.ljudmila.liminoid

import scala.util.Random

import org.lwjgl.opengl.GL11

final object Utils {
  implicit class D(val d: Double) { def prob(): Boolean = util.Random.nextDouble < d } //0.5.prob #syntaxabuse
  implicit class F(val f: Float) { def prob(): Boolean = util.Random.nextFloat < f }
  implicit class I(val i: Int) { 
    def second(): Int = if(i == 1) i.seconds else throw new IllegalArgumentException
    def seconds(): Int = i*1000
  }
  
  def min(a: Double, b: Double, c: Double): Double = math.min(math.min(a, b), c)
  def max(a: Double, b: Double, c: Double): Double = math.max(math.max(a, b), c)
  def pow2(d: Double): Double = d*d
  def getRatio(p: Double): (Double, Double) = (p, 1 - p)
  
  final object SettingsReader {
    type SettingsMap = Map[String, String]
    def apply(str: String): SettingsMap = {
      sealed trait State
      case object OneLine extends State
      case class Text(
          prop: String, 
          buffer: StringBuilder = new StringBuilder) extends State
      
      val settingsMap = 
        str.split("\n")
          .map{ line =>
            line.trim
          }.filterNot{ line => 
            line.isEmpty() || line.startsWith("#") || line.startsWith("//")
          }.foldLeft((Map.empty[String,String], OneLine: State)) {
            case ((map, OneLine), line) =>
              val split = line.split(" *= *")
              val prop = split(0).trim
              val value = split(1).trim.stripPrefix("\"").stripSuffix(",").stripSuffix("\"")
              if (split(1) == "\"\"\"") {
                (map, Text(prop))
              } else {
                (map + (prop -> value), OneLine)
              }
            case ((map, state @ Text(prop, buffer)), line) =>
              if (line == "\"\"\"") {
                (map + (prop -> buffer.toString.trim), OneLine)
              } else {
                buffer.append("\n").append(line)
                (map, state)
              }
          }._1
      
      settingsMap.mapValues { value => "@([a-zA-Z0-9]+)@".r.replaceAllIn(value, m => settingsMap(m.group(1))) }
    }
    def load(fileName: String): SettingsMap = {
      SettingsReader(getFile(fileName).mkString("\n"))
    }
  }
  
  object TableRandom {
    private[this] var index = 0
    private[this] val length = 10000
    private[this] val intTable   = Array.fill(length)(Random.nextInt(length))
    private[this] val gaussTable = Array.fill(length)(Random.nextGaussian)
    def nextGaussian: Double = gaussTable(Random.nextInt(length))
    def nextGaussianUnsafe: Double = { // Thread Unsafe //TODO: Measure with synchronization
      index += 1
      if(index >= length) index = 0
      gaussTable(intTable(index))
    }
  }
  
  
  def withAlternative[T](func: => T, alternative: => T ): T = try { func } catch { case _: Throwable => alternative}
  def withExit[T](func: => T, exit: => Any = { }): T = try { func } catch { case _: Throwable => exit; sys.exit(-1) }

  def thread(x: => Unit): Unit = {
    (new Thread(new Runnable {
      def run(): Unit = { x }
    })).start
  }

  def getFile(name: String): Seq[String] = {
    val file = io.Source.fromFile(name)
    val out = file.getLines.toVector
    file.close
    out
  }

  class TimeLock {
    private var locked = false
    def isLocked: Boolean = {
      if(locked && milliTime-lockTime > lockDuration) locked = false
      
      locked
    }
    
    private def milliTime: Long = System.nanoTime()/1000000L
    
    private var lockTime = milliTime
    private var lockDuration = 0L
    def lockIt(ms: Long): Unit = {
      lockTime = milliTime
      lockDuration = ms
      locked = true
    }
  }
  
  def currentTime: Long = System.nanoTime()
  var timeDivisor = 1000000L //millis
  def since(time: Int): Int = now-time
  def now: Int = (System.nanoTime()/timeDivisor).toInt
  def time(func: => Unit): Int = {
    val startTime = now
    func
    now-startTime
  }

  def pad(i: Int, p: Int = 4): String = "0"*(p-i.toString.size)+i.toString
  val (inf, ninf) = (Double.PositiveInfinity, Double.NegativeInfinity)
}
