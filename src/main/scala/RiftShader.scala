package org.ljudmila.liminoid

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL30._

import java.nio.ByteBuffer
import java.nio.IntBuffer

import org.lwjgl.BufferUtils
import org.lwjgl.LWJGLException
import org.lwjgl.input.Keyboard
import org.lwjgl.opengl._

// Adapted from https://developer.oculusvr.com/forums/viewtopic.php?f=20&t=88&start=10

class RiftShader(screenWidth: Int, screenHeight: Int) {
  sealed trait Eye
  case class Left() extends Eye
  case class Right() extends Eye
  
  val vertexShader = 
    """
    |void main() {
    |   gl_TexCoord[0] = gl_MultiTexCoord0;
    |   gl_Position = gl_Vertex;
    |}
    """.stripMargin
  
  val fragmentShader = 
    """
    |uniform sampler2D tex;
    |uniform vec2 LensCenter;
    |uniform vec2 ScreenCenter;
    |uniform vec2 Scale;
    |uniform vec2 ScaleIn;
    |uniform vec4 HmdWarpParam;
    | 
    |vec2 HmdWarp(vec2 texIn) 
    |{ 
    |   vec2 theta = (texIn - LensCenter) * ScaleIn;
    |   float rSq = theta.x * theta.x + theta.y * theta.y;
    |   vec2 theta1 = theta * (HmdWarpParam.x + HmdWarpParam.y * rSq + HmdWarpParam.z * rSq * rSq + HmdWarpParam.w * rSq * rSq * rSq);
    |   return LensCenter + Scale * theta1;
    |}
    |
    |void main()
    |{
    |   vec2 tc = HmdWarp(gl_TexCoord[0].xy);
    |   if (any(notEqual(clamp(tc, ScreenCenter-vec2(0.25,0.5), ScreenCenter+vec2(0.25, 0.5)) - tc, vec2(0.0, 0.0))))
    |       gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
    |   else
    |       gl_FragColor = texture2D(tex, tc);
    |}
    """.stripMargin

  val (shader, vertShader, fragShader) = initShaders()
  val (colorTextureID, framebufferID, depthRenderBufferID) = initFBO()
  Util.checkGLError()
  
  val LensCenterLocation = glGetUniformLocation(shader, "LensCenter")
  val ScreenCenterLocation = glGetUniformLocation(shader, "ScreenCenter")
  val ScaleLocation = glGetUniformLocation(shader, "Scale")
  val ScaleInLocation = glGetUniformLocation(shader, "ScaleIn")
  val HmdWarpParamLocation = glGetUniformLocation(shader, "HmdWarpParam")
  Util.checkGLError()

  def initFBO(): (Int, Int, Int) = {
    val framebufferID = glGenFramebuffers()
    val colorTextureID = glGenTextures()
    val depthRenderBufferID = glGenRenderbuffers()

    glBindFramebuffer(GL_FRAMEBUFFER, framebufferID)

    // Initialize color texture
    glBindTexture(GL_TEXTURE_2D, colorTextureID)
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, screenWidth, screenHeight, 0,GL_RGBA, GL_INT, null: IntBuffer)
    //glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP)
    //glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP)

    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,GL_TEXTURE_2D, colorTextureID, 0)

    // Initialize depth renderbuffer
    glBindRenderbuffer(GL_RENDERBUFFER, depthRenderBufferID)
    glRenderbufferStorage(GL_RENDERBUFFER, GL14.GL_DEPTH_COMPONENT24, screenWidth, screenHeight)
    glFramebufferRenderbuffer(GL_FRAMEBUFFER,GL_DEPTH_ATTACHMENT,GL_RENDERBUFFER, depthRenderBufferID)

    glBindFramebuffer(GL_FRAMEBUFFER, 0)

    (framebufferID, colorTextureID, depthRenderBufferID)
  }

  def beginOffScreenRenderPass() {        
    glBindTexture(GL_TEXTURE_2D, 0)
    Util.checkGLError()
    glBindFramebuffer(GL_FRAMEBUFFER, framebufferID)
    Util.checkGLError()
  }
  
  def endOffScreenRenderPass() {
    // nop
  }
  
  def renderToScreen() {
    Util.checkGLError()
    glUseProgram(shader)
    Util.checkGLError()
    
    glEnable(GL_TEXTURE_2D)
    glDisable(GL_DEPTH_TEST)
    glBindFramebuffer(GL_FRAMEBUFFER, 0)

    glClearColor (1.0f, 0.0f, 0.0f, 0.5f)
    glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    glBindTexture(GL_TEXTURE_2D, colorTextureID);  

    renderDistortedEye(Left(), 0.0f, 0.0f, 0.5f, 1.0f)
    renderDistortedEye(Right(), 0.5f, 0.0f, 0.5f, 1.0f)

    glUseProgram(0)
    glEnable(GL_DEPTH_TEST)
  }
  
  val K0 = 1.0f
  val K1 = 0.22f
  val K2 = 0.24f
  val K3 = 0.0f
  
  def renderDistortedEye(eye: Eye, x: Float, y: Float, w: Float, h: Float) {
    val as = w/h
    val scaleFactor = 1.0f
    val eyeHoleScale = 1.75f
    
    this.validate()
    Util.checkGLError()
    
    val DistortionXCenterOffset = eye match {
      case Left()  => +0.25f
      case Right() => -0.25f
    }
    
    glUniform2f(LensCenterLocation, x + (w + DistortionXCenterOffset * 0.5f)*0.5f, y + h*0.5f)
    glUniform2f(ScreenCenterLocation, x + w*0.5f, y + h*0.5f)
    glUniform2f(ScaleLocation, (w/2.0f) * scaleFactor, (h/2.0f) * scaleFactor * as)
    glUniform2f(ScaleInLocation, (eyeHoleScale/w), (eyeHoleScale/h) / as)

    glUniform4f(HmdWarpParamLocation, K0, K1, K2, K3)
    
    glBegin(GL_TRIANGLE_STRIP)
    eye match {
      case Left() =>
        glTexCoord2f(0.0f, 0.0f); glVertex2f(-1.0f, -1.0f)
        glTexCoord2f(0.5f, 0.0f); glVertex2f( 0.0f, -1.0f)
        glTexCoord2f(0.0f, 1.0f); glVertex2f(-1.0f, +1.0f)
        glTexCoord2f(0.5f, 1.0f); glVertex2f( 0.0f, +1.0f)
      case Right() =>
        glTexCoord2f(0.5f, 0.0f); glVertex2f(0.0f, -1.0f)
        glTexCoord2f(1.0f, 0.0f); glVertex2f(1.0f, -1.0f)
        glTexCoord2f(0.5f, 1.0f); glVertex2f(0.0f, +1.0f)
        glTexCoord2f(1.0f, 1.0f); glVertex2f(1.0f, +1.0f)
    }
    glEnd
  }
  
  def initShaders(): (Int, Int, Int) = {
    val shader = glCreateProgram()

    val vertShader = createVertShader(vertexShader)
    val fragShader = createFragShader(fragmentShader)
    Util.checkGLError()

    if(vertShader != 0 && fragShader != 0) {
      glAttachShader(shader, vertShader)
      glAttachShader(shader, fragShader)

      glLinkProgram(shader)
      if(glGetProgrami(shader, GL_LINK_STATUS) == GL_FALSE) {
        println("Linkage error")
        printLogInfo(shader)
      }

      glValidateProgram(shader)
      if(glGetProgrami(shader, GL_VALIDATE_STATUS) == GL_FALSE) {
        println("Validation error")
        printLogInfo(shader)
      }
    } else {
      println("No shaders")
    }
    Util.checkGLError()

    (shader, vertShader, fragShader)
  }

  def validate() {
    glValidateProgram(shader)
    if(glGetProgrami(shader, GL_VALIDATE_STATUS) == GL_FALSE) {
      printLogInfo(shader)
    }
  }

  def createVertShader(vertexCode: String): Int = {
    val vertShader = glCreateShader(GL_VERTEX_SHADER)

    if(vertShader != 0) {
      glShaderSource(vertShader, vertexCode)
      glCompileShader(vertShader)

      if(glGetShaderi(vertShader, GL_COMPILE_STATUS) == GL_FALSE) {
        printLogInfo(vertShader)
        return 0
      }
    }
    
    vertShader
  }

  def createFragShader(fragCode: String): Int = {
    val fragShader = glCreateShader(GL_FRAGMENT_SHADER)
    if (fragShader != 0) {
      glShaderSource(fragShader, fragCode)
      glCompileShader(fragShader)
      if (glGetShaderi(fragShader, GL_COMPILE_STATUS) == GL_FALSE) {
        printLogInfo(fragShader)
        return 0
      }
    }
    
    fragShader
  }

  def printLogInfo(obj:Int) {
    val iVal = BufferUtils.createIntBuffer(1)
    glGetShader(obj, GL_INFO_LOG_LENGTH, iVal)

    val length = iVal.get()
    if(length > 1) {
      val infoLog = BufferUtils.createByteBuffer(length)
      iVal.flip()
      glGetShaderInfoLog(obj, iVal, infoLog)
      val infoBytes = Array.ofDim[Byte](length)
      infoLog.get(infoBytes)
      val out = new String(infoBytes)
      println("Info log:\n" + out)
    }
  }
}
