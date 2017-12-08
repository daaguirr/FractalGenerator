package GUI

import java.io.File
import javafx.scene.image.PixelWriter

import Model._

import scalafx.application.JFXApp
import scalafx.embed.swing.SwingFXUtils
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import javax.imageio.ImageIO
import java.awt.image.RenderedImage

import scalafx.scene.image.WritableImage

object Canvas extends JFXApp {
  val global_width : Int = 1366
  val global_height : Int = 768
  var matrix: Array[Array[Color]] = Array.ofDim[Color](global_width,global_height)

  val color : ColorPalette = new PrettyPalette()
  var fractal: Fractal = new Mandelbrot(global_width, global_height, color)
  fractal = new GeneralizedJulia(global_width,global_height,color, c => c*c*c.exp + new Complex(0.21,0) )
  //fractal = new Julia(global_width, global_height, color,new Complex(-0.8,0.156))

  fractal.generate(100, 0, 0, 2)
  matrix = fractal.matrix


  stage = new JFXApp.PrimaryStage {
    title.value = "Fractal Generator 2.0"
    scene = new Scene() {
      fill = White
      val canvas = new Canvas(global_width,global_height)
      val gc: GraphicsContext = canvas.graphicsContext2D
      val plume: PixelWriter = gc.pixelWriter
      for (i <- 0 until global_width){
        for (j <- 0 until global_height){
          plume.setColor(i,j,matrix(i)(j))
        }
      }

      val file : File = new File("save.png")
      val writableImage = new WritableImage(global_width, global_height)
      canvas.snapshot(null, writableImage)
      val renderedImage: RenderedImage = SwingFXUtils.fromFXImage(writableImage, null)
      ImageIO.write(renderedImage, "png", file)
      content = canvas
    }
  }

}