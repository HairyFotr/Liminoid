package org.ljudmila.liminoid

import scala.util.Random

final object Utils {
  /*def cleanBuffer(b: java.io.ByteBuffer) {
    import sun.misc.Cleaner
    import java.lang.reflect.Field

    val cleanerField: Field = b.getClass.getDeclaredField("cleaner")
    cleanerField.setAccessible(true)
    val cleaner: Cleaner = cleanerField.get(b).asInstanceOf[Cleaner]
    cleaner.clean
  }*/

  implicit class D(val d: Double) { def prob(): Boolean = util.Random.nextDouble < d } //0.5.prob #syntaxabuse
  implicit class F(val f: Float) { def prob(): Boolean = util.Random.nextFloat < f }

  @inline def min(a: Double, b: Double, c: Double): Double = math.min(math.min(a, b), c)
  @inline def max(a: Double, b: Double, c: Double): Double = math.max(math.max(a, b), c)
  
  object TableRandom {
    private[this] var index = 0
    private[this] val length = 10000
    private[this] val intTable   = Array.fill(length)(Random.nextInt(length))
    private[this] val gaussTable = Array.fill(length)(Random.nextGaussian)
    @inline def nextGaussian: Double = gaussTable(Random.nextInt(length))
    @inline def nextGaussian2: Double = {
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
