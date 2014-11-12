package org.ljudmila

import scala.util.Random
import scala.util.Random._

final object Utils {
  implicit class D(val d: Double) { def prob(): Boolean = util.Random.nextDouble < d } //0.5.prob #syntaxabuse
  implicit class F(val f: Float) { def prob(): Boolean = util.Random.nextFloat < f }
  implicit class I(val i: Int) { // to/from ms 
    def second(): Int = if(i == 1) i.seconds else throw new IllegalArgumentException
    def seconds(): Int = i*1000
    def FPS(): Double = 1000d/i
  }
  
  def min(a: Double, b: Double, c: Double): Double = math.min(math.min(a, b), c)
  def max(a: Double, b: Double, c: Double): Double = math.max(math.max(a, b), c)
  def pow2(d: Double): Double = d*d
  def getRatio(p: Double): (Double, Double) = (p, 1 - p)
  
  def angleDist(a1: Double, a2: Double) = {
    var d = a1-a2
    if (math.abs(d) > 180) d = 360 - d
    d % 360
  }
  def angleAvg(a1: Double, a2: Double) = {
    a1 + angleDist(a1, a2)/2
  }
  def angleAvgW(a: Double, b: Double, ratio: Double): Double = {
      var x = math.abs(a-b) % 360
      if(x >=0 && x <= 180)
         (a*ratio + b*(1-ratio)) % 360
      else if(x > 180 && x < 270)
         (((a*ratio + b*(1-ratio))) % 360) + 180
      else
         (((a*ratio + b*(1-ratio))) % 360) - 180
  }
  
  implicit class Seqs[A](val s: Seq[A]) { 
    def random: A = s(nextInt(s.size))
    def randomOption: Option[A] = if(s.isEmpty) None else Some(random)
  }

  object TableRandom {
    private[this] var index = 0
    private[this] val length = 10000
    private[this] val intTable   = Array.fill(length)(Random.nextInt(length))
    private[this] val gaussTable = Array.fill(length)(Random.nextGaussian)
    private[this] val doubleTable = Array.fill(length)(Random.nextDouble)
    
    def nextGaussian: Double = gaussTable(Random.nextInt(length))
    def nextGaussianUnsafe: Double = { // Thread Unsafe //TODO: Measure with synchronization
      index += 1
      if(index >= length) index = 0
      gaussTable(intTable(index))
    }
    def nextDouble: Double = doubleTable(Random.nextInt(length))
    def nextDoubleUnsafe: Double = { // Thread Unsafe //TODO: Measure with synchronization
      index += 1
      if(index >= length) index = 0
      doubleTable(intTable(index))
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
