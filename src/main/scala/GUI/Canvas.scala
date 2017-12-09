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
import scalafx.scene.control.ChoiceDialog
import scalafx.scene.image.WritableImage
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._

object Canvas extends JFXApp {
  // TO EDIT OR ADD FRACTALS SEE A selectChoiceFractal

  // Global Settings
  var global_width: Int = 640
  var global_height: Int = 480
  var matrix: Array[Array[Color]] = Array.ofDim[Color](global_width, global_height)
  val color: ColorPalette = new PrettyPalette
  var fractal: Fractal = new Mandelbrot(global_width, global_height, color)

  // Fractal Settings
  var currentOffX = 0d
  var currentOffY = 0d
  var currentScale = 1
  var currentItersHD = 40
  var currentItersLC = 10
  var lastX = 0d
  var lastY = 0d

  // GUI elements
  val canvas = new Canvas(global_width, global_height)
  val gc: GraphicsContext = canvas.graphicsContext2D
  val plume: PixelWriter = gc.pixelWriter

  // Canvas Init
  stage = new JFXApp.PrimaryStage {
    title.value = "Fractal Generator 2.0"
    scene = new Scene() {
      fill = White
      choiceFractalDialog()
      choiceColorDialog()
      generateFractal(currentItersHD)
      content = canvas
    }
  }

  /***
    *  Starting Dragging, set initial drag point
    */
  canvas.setOnDragDetected(event => {
    lastX = event.getSceneX
    lastY = event.getSceneY
    event.consume()
  })

  /***
    * Generate pre-visualization when the user move the fractal
    */
  canvas.setOnMouseDragged(event => {
    gc.clearRect(0, 0, canvas.getWidth, canvas.getHeight) // Clean Canvas
    currentOffX -= (1.0/currentScale)*0.3*(event.getSceneX - lastX) / global_width
    currentOffY -= (1.0/currentScale)*0.3*(event.getSceneY - lastY) / global_height
    generateFractal(currentItersLC)
    event.consume()
  })

  /***
    * Drag is Done, generate full quality fractal
    */
  canvas.setOnMouseReleased(event => {
    gc.clearRect(0, 0, canvas.getWidth, canvas.getHeight) // Clean Canvas
    generateFractal(currentItersHD)
    event.consume()
  })

  /***
    * KeyPress Handler {Zoom , ScreenShot}
    */
  stage.getScene.setOnKeyPressed(event => {
    event.getCode match {
      case KeyCode.W => // Zoom In
        currentItersHD+=3
        currentItersLC+=3
        currentScale *= 2
      case down if down == KeyCode.S && currentScale > 1 => // Zoom Out
        currentScale /= 2
        currentItersHD-=3
        currentItersLC-=3
      case KeyCode.P => saveImage() // ScreenShot
      case _ =>
    }
    println(currentScale)
    generateFractal(currentItersHD)
    event.consume()
  })

  /***
    * Generate fractal using the canvas
    * @param iters detail of generation
    */
  def generateFractal(iters : Int): Unit ={
    fractal.generate(iters, currentOffX , currentOffY , currentScale)
    matrix = fractal.matrix
    for (i <- 0 until global_width) {
      for (j <- 0 until global_height) {
        plume.setColor(i, j, matrix(i)(j))
      }
    }
  }

  /***
    * Save a screenshot of canvas
    */
  def saveImage(): Unit = {
    val file: File = new File("save.png")
    val writableImage = new WritableImage(global_width, global_height)
    canvas.snapshot(null, writableImage)
    val renderedImage: RenderedImage = SwingFXUtils.fromFXImage(writableImage, null)
    ImageIO.write(renderedImage, "png", file)
  }

  /***
    * Select fractal dialog
    */
  def choiceFractalDialog(): Unit = {
    val choices = Seq("Mandelbrot", "Julia", "Generalized Julia")
    val dialog = new ChoiceDialog(defaultChoice = "Maldelbrot", choices = choices) {
      initOwner(stage)
      title = "Fractal Generator"
      headerText = "Fractal Generator"
      contentText = "Choose your Fractal:"
    }
    val result = dialog.showAndWait()
    result match {
      case Some(choice) => selectChoiceFractal(choice)
      case None         => System.exit(0)
    }
  }

  /**
    * Change fractal variable by a new fractal of choice
    * @param choice {Mandelbrot,Julia,Generalized Julia}
    */
  def selectChoiceFractal(choice : String): Unit ={
    choice match {
      case "Mandelbrot" => fractal = new Mandelbrot(global_width, global_height, color)
      case "Julia" => fractal = new Julia(global_width, global_height, color,new Complex(-0.8,0.156))
      case "Generalized Julia" => fractal= new GeneralizedJulia(global_width, global_height, color, c => c * c * c.exp + new Complex(0.21, 0))
      case _ =>
    }
  }

  /***
    * Select color dialog
    */
  def choiceColorDialog(): Unit = {
    val choices = Seq("Cold", "Hot", "Pretty")
    val dialog = new ChoiceDialog(defaultChoice = "Pretty", choices = choices) {
      initOwner(stage)
      title = "Fractal Generator"
      headerText = "Fractal Generator"
      contentText = "Choose your Color Palette:"
    }
    val result = dialog.showAndWait()
    result match {
      case Some(choice) => selectChoiceColor(choice)
      case None         => System.exit(0)
    }
  }

  /**
    * Change color fractal variable by a new color
    * @param choice {Cold, Hot, Pretty}
    */
  def selectChoiceColor(choice : String): Unit ={
    choice match {
      case "Cold" => fractal.colorPalette = new ColdPalette
      case "Hot" => fractal.colorPalette = new HotPalette
      case "Pretty" => fractal.colorPalette = new PrettyPalette
      case _ =>
    }
  }

}