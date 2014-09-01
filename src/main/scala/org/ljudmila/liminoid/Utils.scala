package org.ljudmila.liminoid

import scala.util.Random
import scala.util.Random._

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
  
  implicit class Seqs[A](val s: Seq[A]) { 
    def random: A = s(nextInt(s.size))
    def randomOption: Option[A] = if(s.isEmpty) None else Some(random)
  }
  
  final object SettingsReader {
    type SettingsMap = Map[String, String]
    def apply(str: String): SettingsMap = {
      sealed trait State
      case object OneLine extends State
      case class Text(
          prop: String, 
          buffer: StringBuilder = new StringBuilder) extends State
      case class Comment(prevState: State) extends State
      
      var settingsMap = 
        str.split("\n")
          .map{ line =>
            line.trim
          }.filterNot{ line => 
            line.isEmpty() || line.startsWith("#") || line.startsWith("//")
          }.foldLeft((Map.empty[String,String], OneLine: State)) {
            case ((map, comment @ Comment(prevState)), line) =>
              if (line.trim endsWith "*/") {
                (map, prevState)
              } else {
                (map, comment)
              }
            case ((map, state), line) if (line.trim startsWith "/*") =>
              (map, Comment(state))              
            case ((map, OneLine), line) =>
          	  val split = line.split(" *= *")
              if (split.size == 1) throw new Exception("Syntax error on: " + line)
              val prop = split(0).trim
              val value = split(1).trim.stripPrefix("\"").stripSuffix(",").stripSuffix("\"")
              if (split(1) == "\"\"\"") {
                (map, Text(prop))
              } else {
                (map + (prop -> value), OneLine)
              }
            case ((map, state @ Text(prop, buffer)), line) =>
              if (line.trim == "\"\"\"") {
                (map + (prop -> buffer.toString.trim), OneLine)
              } else {
                buffer.append("\n").append(line)
                (map, state)
              }
          }._1
      
      // Replace      
      settingsMap = settingsMap.mapValues { value =>
        "@([a-zA-Z0-9]+)@".r.replaceAllIn(value, m => settingsMap(m.group(1)))
      }

      // Loop
      settingsMap = settingsMap.mapValues { value =>
        "(?s)[%][(]([0-9]+)[)](.+?)[%]".r.replaceAllIn(value, m => {
          val strB = new StringBuilder
          val n = m.group(1).toInt
          for(i <- 0 until n) {
            strB.append(
              m.group(2)
                .replace("{i}", i.toString)
                .replace("{iC}", (2*(i/n.toDouble)*math.Pi).toString))
          }
          strB.toString
        })
      }
      
      // Calculate
      val funcMap = Map[String, Array[String] => String](
          "rand"   -> (a => ((nextDouble*2 - 1) * a(0).toDouble).toString),
          "choose" -> (a => a.toSeq.random),
          "sin"    -> (a => math.sin(a(0).toDouble).toString),
          "cos"    -> (a => math.cos(a(0).toDouble).toString))
          
      settingsMap = settingsMap.mapValues { value => 
        "[{]([a-zA-Z0-9]+)[(]([^)]+)[)][}]".r.replaceAllIn(value, m => funcMap(m.group(1))(m.group(2).split(" *, *")))
      }

      val infixFuncMap = Map[String, (String, String) => String](
          "-" -> ((a, b) => (a.toDouble - b.toDouble).toString),
          "+" -> ((a, b) => (a.toDouble + b.toDouble).toString),
          "*" -> ((a, b) => (a.toDouble * b.toDouble).toString),
          "/" -> ((a, b) => (a.toDouble / b.toDouble).toString))
          
      settingsMap = settingsMap.mapValues { value => 
        "[{]([-+]?[0-9.E-]+) *([-+*/]) *([-+]?[0-9.E-]+)[}]".r.replaceAllIn(value, m => infixFuncMap(m.group(2))(m.group(1), m.group(3)))
      }
      
      settingsMap
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
