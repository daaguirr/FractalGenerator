package Model

import java.util.concurrent.{CountDownLatch, ExecutorService, Executors}

import scalafx.scene.paint.Color

/**
  * Created by dagum on 08-12-2017.
  */
abstract class Fractal(var _width: Int, var _height: Int, var colorPalette: ColorPalette) {
  var matrix: Array[Array[Color]] = Array.ofDim[Color](_width, _height)
  val pool: ExecutorService = Executors.newFixedThreadPool(4)
  /** *
    * Generate matrix color representing fractal
    * @param iters max number of iters
    * @param x_offset x axis offset [-1,1] (reverse)
    * @param y_offset y axis offset [-1,1] (reverse)
    * @param scale zoom [1,infinity]
    */
  def generate(iters: Int, x_offset: Double, y_offset: Double, scale: Double): Unit = {
    // TODO: fix overflow bug
    val latch = new CountDownLatch(_height*_width)
    for (iy <- 0 until _height) { // print 10% step
      for (ix <- 0 until _width) {
        pool.execute(() => {
          latch.countDown()
          val (z, iter) = getVal(ix * 1.0 / _width, iy * 1.0 / _height, x_offset, y_offset, scale, iters) // evaluate function
          val color = colorPalette.get(iter, iters, z) // get color of result
          setMatrix(ix, iy, Color.rgb(color._1, color._2, color._3)) // set color
        })

      }
    }
    latch.await()
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

  /**
    * Thread Safe matrix setter
    * @param x x position on matrix
    * @param y y position on matrix
    * @param color new color
    */
  private def setMatrix(x : Int,y : Int,color: Color): Unit = matrix.synchronized {
    matrix(x)(y) = color
  }
}
