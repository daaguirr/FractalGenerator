package Model

import scalafx.scene.paint.Color

/**
  * Created by dagum on 08-12-2017.
  */
abstract class Fractal(var _width: Int, var _height: Int, val colorPalette: ColorPalette) {
  var matrix: Array[Array[Color]] = Array.ofDim[Color](_width, _height)

  /** *
    * @param iters
    * @param x_offset -0.5 - 0.5 orden inverso
    * @param y_offset -0.5 - 0.5 orden normal
    * @param scale
    */
  def generate(iters: Int, x_offset: Double, y_offset: Double, scale: Double): Unit = {
    var next = 10
    for (iy <- 0 until _height) {
      if (iy * 100.0 / _height > next) {
        println(next + "%")
        next += 10
      }
      for (ix <- 0 until _width) {
        val (z, iter) = getVal(ix * 1.0 / _width, iy * 1.0 / _height, x_offset, y_offset, scale, iters)
        val color = colorPalette.get(iter, iters, z)
        matrix(ix)(iy) = Color.rgb(color._1, color._2, color._3)
      }
    }
  }

  def getVal(ix: Double, iy: Double, x_offset: Double, y_offset: Double, scale: Double, max_iters: Int): (Complex, Int)

}
