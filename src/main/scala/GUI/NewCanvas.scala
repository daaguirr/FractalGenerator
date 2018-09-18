package GUI

import Model._
import javafx.scene.image.PixelWriter
import javafx.scene.input.KeyCode
import scalafx.application.{JFXApp, Platform}
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._

object NewCanvas extends JFXApp {
  // TO EDIT OR ADD FRACTALS SEE A selectChoiceFractal

  // Global Settings
  var global_width: Int = 640
  var global_height: Int = 480
  var matrix: Array[Array[Color]] = Array.ofDim[Color](global_width, global_height)
  val color: ColorPalette = new PrettyPalette
  var fractal: NewMandelbrot = new NewMandelbrot(global_width, global_height, color)

  // Fractal Settings
  var currentScale: Int = 1
  var currentItersHD = 100
  var currentItersLC = 10
  var lastX: Double = global_width/2
  var lastY: Double = global_height/2

  // GUI elements
  val canvas = new Canvas(global_width, global_height)
  val gc: GraphicsContext = canvas.graphicsContext2D
  val plume: PixelWriter = gc.pixelWriter

  // Canvas Init
  stage = new JFXApp.PrimaryStage {
    title.value = "Fractal Generator 2.0"
    scene = new Scene() {
      fill = White
      generateFractal(currentItersHD)
      content = canvas
    }
  }


  stage.getScene.setOnMouseMoved(event => {
    lastX = event.getSceneX
    lastY = event.getSceneY
  })

  stage.getScene.setOnKeyPressed(event => {
    event.getCode match {
      case KeyCode.W => // Zoom In
        currentItersHD += 10
        currentScale += 1
      case down if down == KeyCode.S && currentScale > 1 => // Zoom Out
        currentScale -= 1
        currentItersHD -= 10
      case _ =>
    }
    println(currentScale)
    generateFractal(currentItersHD)
    event.consume()
  })

  def generateFractal(iters: Int): Unit = {
    fractal.generate(iters, lastX, lastY, currentScale)
    matrix = fractal.matrix
    for (i <- 0 until global_width) {
      for (j <- 0 until global_height) {
        plume.setColor(i, j, matrix(i)(j))
      }
    }
  }

  stage.setOnCloseRequest(_ => {
    fractal.pool.shutdown()
    Platform.exit
    System.exit(0)
  })

}