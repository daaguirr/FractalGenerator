package Model

import scalafx.scene.paint.Color

/**
  * Created by dagum on 08-12-2017.
  */
class Mandelbrot(_width : Int, _height : Int, colorPalette: ColorPalette) extends Fractal(_width,_height, colorPalette){
  override def getVal(ix: Double, iy: Double, x_offset: Double, y_offset: Double, scale: Double, max_iters: Int): (Complex, Int) = {
    val x_tmp = 0.5 - x_offset + (ix - 0.5) / scale
    val y_tmp = 0.5 - y_offset + (iy - 0.5) / scale

    val x0 = -2.5 + x_tmp * 3.5
    val y0 = -1 + 2.0 * y_tmp

    var z = new Complex(0,0)
    var iters = 0

    while (z.radius < 4 && iters < max_iters) {
      z = z * z + new Complex(x0, y0)
      iters += 1
    }
    (z, iters)
  }

}
