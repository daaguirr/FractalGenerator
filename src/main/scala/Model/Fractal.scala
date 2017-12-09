package Model

import scalafx.scene.paint.Color

/**
  * Created by dagum on 08-12-2017.
  */
abstract class Fractal(var _width: Int, var _height: Int, var colorPalette: ColorPalette) {
  var matrix: Array[Array[Color]] = Array.ofDim[Color](_width, _height)

  /** *
    * Generate matrix color representing fractal
    * @param iters max number of iters
    * @param x_offset x axis offset [-1,1] (reverse)
    * @param y_offset y axis offset [-1,1] (reverse)
    * @param scale zoom [1,infinity]
    */
  def generate(iters: Int, x_offset: Double, y_offset: Double, scale: Double): Unit = {
    // TODO: fix overflow bug
    var next = 10
    for (iy <- 0 until _height) { // print 10% step
      if (iy * 100.0 / _height > next) {
        println(next + "%")
        next += 10
      }
      for (ix <- 0 until _width) {
        val (z, iter) = getVal(ix * 1.0 / _width, iy * 1.0 / _height, x_offset, y_offset, scale, iters) // evaluate function
        val color = colorPalette.get(iter, iters, z) // get color of result
        matrix(ix)(iy) = Color.rgb(color._1, color._2, color._3) // set color
      }
    }
  }

  /***
    * Aux function that calculate a function over a point (fractal on a point) with params
    * @param ix x position on canvas
    * @param iy y position on canvas
    * @param x_offset x axis offset [-1,1] (reverse)
    * @param y_offset y axis offset [-1,1] (reverse)
    * @param scale zoom [1,infinity]
    * @param max_iters max number of iters
    * @return Tuple2 representing a complex on a point (function eval) and used iterations
    */
  def getVal(ix: Double, iy: Double, x_offset: Double, y_offset: Double, scale: Double, max_iters: Int): (Complex, Int)

}
