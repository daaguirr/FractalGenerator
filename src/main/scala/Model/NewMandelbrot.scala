package Model

import java.util.concurrent.{CountDownLatch, ExecutorService, Executors}

import scalafx.scene.paint.Color

class NewMandelbrot(_width: Int, _height: Int, colorPalette: ColorPalette) {
  var matrix: Array[Array[Color]] = Array.ofDim[Color](_width, _height)
  val pool: ExecutorService = Executors.newFixedThreadPool(4)
  var xa: Double = -2.5
  var xb: Double = 1.0
  var ya: Double = -1.0
  var yb: Double = 1.0
  var last_scale: Int = 1

  def getVal(ix: Double, iy: Double, x_offset: Double, y_offset: Double, scale: Double, max_iters: Int): (Complex, Int) = {
    val x0 = xa + (xb - xa) * ix / _width
    val y0 = ya + (yb - ya) * iy / _height

    val c = new Complex(x0, y0)
    var z = new Complex(0, 0)

    var iters = 0

    while (z.radius < 4 && iters < max_iters) {
      z = z * z + c
      iters += 1
    }
    (z, iters)
  }

  def generate(iters: Int, x_offset: Double, y_offset: Double, scale: Int): Unit = {
    // TODO: fix overflow bug
    val deltax = math.abs(xb - xa)
    val deltay = math.abs(yb - ya)

    val xoff = x_offset - _width / 2
    val yoff = y_offset - _height / 2


    var sgn = scale - last_scale
    if (sgn < 0) sgn *= 2

    xa = xa + xoff * deltax / _width + sgn * deltax / 4
    xb = xb + xoff * deltax / _width - sgn * deltax / 4

    ya = ya + yoff * deltay / _height + sgn * deltay / 4
    yb = yb + yoff * deltay / _height - sgn * deltay / 4

    last_scale = scale


    val latch = new CountDownLatch(_height * _width)
    for (iy <- 0 until _height) { // print 10% step
      for (ix <- 0 until _width) {
        pool.execute(() => {
          latch.countDown()
          val (z, iter) = getVal(ix * 1.0, iy * 1.0, x_offset, y_offset, scale, iters) // evaluate function
          val color = colorPalette.get(iter, iters, z) // get color of result
          setMatrix(ix, iy, Color.rgb(color._1, color._2, color._3)) // set color
        })

      }
    }
    latch.await()
  }

  private def setMatrix(x: Int, y: Int, color: Color): Unit = matrix.synchronized {
    matrix(x)(y) = color
  }
}