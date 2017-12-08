package Model

/**
  * Created by dagum on 08-12-2017.
  */
class Julia(_width : Int, _height : Int, colorPalette: ColorPalette, complex: Complex) extends Fractal(_width,_height, colorPalette){

  override def getVal(ix: Double, iy: Double, x_offset: Double, y_offset: Double, scale: Double, max_iters: Int): (Complex, Int) = {
    var z = new Complex((ix * 3 - 1.5 + x_offset)/scale, (iy * 3 - 1.5 + y_offset)/scale)
    var iters = 0

    while (z.radius < 10 && iters < max_iters) {
      z = z * z + complex
      iters += 1
    }
    (z, iters)
  }
}
