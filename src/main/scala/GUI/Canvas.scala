package GUI

import java.awt.image.RenderedImage
import java.io.File
import javafx.scene.image.PixelWriter
import javafx.scene.input.KeyCode
import javax.imageio.ImageIO

import Model._

import scalafx.application.JFXApp
import scalafx.embed.swing.SwingFXUtils
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.image.WritableImage
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._

object Canvas extends JFXApp {
  val global_width: Int = 640
  val global_height: Int = 480
  var matrix: Array[Array[Color]] = Array.ofDim[Color](global_width, global_height)

  val color: ColorPalette = new PrettyPalette()
  var fractal: Fractal = new Mandelbrot(global_width, global_height, color)
  //fractal = new GeneralizedJulia(global_width, global_height, color, c => c * c * c.exp + new Complex(0.21, 0))
  //fractal = new Julia(global_width, global_height, color,new Complex(-0.8,0.156))

  var currentOffX = 0d
  var currentOffY = 0d
  var currentScale = 1
  var currentItersHD = 40
  var currentItersLC = 10
  var lastX = 0d
  var lastY = 0d
  val canvas = new Canvas(global_width, global_height)
  val gc: GraphicsContext = canvas.graphicsContext2D
  val plume: PixelWriter = gc.pixelWriter

  generateFractal(currentItersHD)

  stage = new JFXApp.PrimaryStage {
    title.value = "Fractal Generator 2.0"
    scene = new Scene() {
      fill = White
      val file: File = new File("save.png")
      val writableImage = new WritableImage(global_width, global_height)
      canvas.snapshot(null, writableImage)
      val renderedImage: RenderedImage = SwingFXUtils.fromFXImage(writableImage, null)
      ImageIO.write(renderedImage, "png", file)
      content = canvas
    }
  }

  canvas.setOnDragDetected(event => {
    lastX = event.getSceneX
    lastY = event.getSceneY
    event.consume()
  })

  canvas.setOnMouseDragged(event => {
    gc.clearRect(0, 0, canvas.getWidth, canvas.getHeight)
    currentOffX -= (1.0/currentScale)*0.3*(event.getSceneX - lastX) / global_width
    currentOffY -= (1.0/currentScale)*0.3*(event.getSceneY - lastY) / global_height
    generateFractal(currentItersLC)
    event.consume()
  })

  canvas.setOnMouseReleased(event => {
    gc.clearRect(0, 0, canvas.getWidth, canvas.getHeight)
    generateFractal(currentItersHD)
    event.consume()
  })


  stage.getScene.setOnKeyPressed(event => {
    val code = event.getCode
    if(code == KeyCode.UP) {
      currentItersHD+=3
      currentItersLC+=3
      currentScale *= 2
    }
    else if (code == KeyCode.DOWN && currentScale > 1) {
      currentScale /= 2
      currentItersHD-=3
      currentItersLC-=3
    }
    println(currentScale)
    generateFractal(currentItersHD)
    event.consume()
  })

  def generateFractal(iters : Int): Unit ={
    fractal.generate(iters, currentOffX , currentOffY , currentScale)
    matrix = fractal.matrix
    for (i <- 0 until global_width) {
      for (j <- 0 until global_height) {
        plume.setColor(i, j, matrix(i)(j))
      }
    }
  }
}