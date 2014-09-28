package org.ljudmila

import scala.util.Random._
import Utils._

import scala.collection.immutable.ListMap

final object SettingsReader {
  type SettingsMap = Map[String, String]
  def apply(str: String): SettingsMap = {
    sealed trait State
    case object OneLine extends State
    case class Text(
        prop: String, 
        buffer: StringBuilder = new StringBuilder) extends State
    case class Comment(
        prevState: State,
        depth: Int = 1) extends State

    var settingsMap: SettingsMap = 
      str.split("\n")
        .map{ line =>
          line.trim
        }.filterNot{ line => 
          line.isEmpty() || line.startsWith("#") || line.startsWith("//")
        }.foldLeft((ListMap.empty[String,String], OneLine: State)) {
          case ((map, comment @ Comment(prevState, depth)), line) =>
            if (line.trim endsWith "*/") {
              if(comment.depth == 0) { 
                (map, prevState)
              } else { 
                (map, comment.copy(depth = comment.depth - 1))
              }
            } else if (line.trim startsWith "/*") {
              (map, comment.copy(depth = comment.depth + 1))
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
      "@([a-zA-Z0-9]+)@".r.replaceAllIn(value, m => 
          if(settingsMap contains m.group(1)) settingsMap(m.group(1))
          else m.group(0))
    }

    // Loop
    settingsMap = settingsMap.mapValues { value =>
      "(?s)[%][(]([0-9]+|[a-zA-Z, ]+)[)](.+?)[%]".r.replaceAllIn(value, m => {
        val strB = new StringBuilder
        val toReg = " *([.][.]|[-=]+>|to) *"
        val elts = 
          (if (m.group(1) matches "[0-9]+") {
            (0 until m.group(1).toInt).map { _.toString }.toArray
          } else if (m.group(1) matches "[0-9]+"+toReg+"[0-9]+") {
            val ns = m.group(1).split(toReg)
            (ns(0).toInt to ns(1).toInt).map { _.toString }.toArray 
          } else {
            m.group(1).split(" *, *")
          })

        for((elt, i) <- elts.zipWithIndex) {
          strB.append(
            m.group(2)
              .replace("@elt@", elt)
              .replace("@i@", i.toString)
              .replace("@iC@", (2*(i/elts.size.toDouble)*math.Pi).toString))
        }
        strB.toString
      })
    }

    // Calculate
    for(iters <- 1 to 10) {
      val infixFuncMap = Map[String, (String, String) => String](
          "-"   -> ((a, b) => (a.toDouble - b.toDouble).toString),
          "+"   -> ((a, b) => (a.toDouble + b.toDouble).toString),
          "*"   -> ((a, b) => (a.toDouble * b.toDouble).toString),
          "/"   -> ((a, b) => (a.toDouble / b.toDouble).toString),
          "mod" -> ((a, b) => (a.toInt % b.toInt).toString))

      settingsMap = settingsMap.mapValues { value => 
        "[{]([-+]?[0-9.E-]+) *([-+*/]|mod) *([-+]?[0-9.E-]+)[}]".r.replaceAllIn(value, m => infixFuncMap(m.group(2))(m.group(1), m.group(3)))
      }

      // TODO map => case partial map for added parsing
      val funcMap = Map[String, Array[String] => String](
          "rand"   -> (a => ((nextDouble*2 - 1) * a(0).toDouble).toString),
          "choose" -> (a => a.toSeq.random),
          "sin"    -> (a => math.sin(a(0).toDouble).toString),
          "cos"    -> (a => math.cos(a(0).toDouble).toString),
          "prob"   -> (a => if (a(0).toDouble.prob) a(1) else a(2)),
          "select" -> (a => a(a(0).toInt + 1)))

      settingsMap = settingsMap.mapValues { value => 
        "[{]([a-zA-Z0-9]+)[(]([^)]+)[)][}]".r.replaceAllIn(value, m => funcMap(m.group(1))(m.group(2).split(" *, *")))
      }
    }
    
    settingsMap
  }
  def load(fileName: String): SettingsMap = {
    SettingsReader(getFile(fileName).mkString("\n"))
  }
}
