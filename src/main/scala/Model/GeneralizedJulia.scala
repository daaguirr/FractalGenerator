package Model

/**
  * Created by dagum on 08-12-2017.
  */
class GeneralizedJulia(_width : Int, _height : Int, colorPalette: ColorPalette, function: Function[Complex,Complex]) extends Fractal(_width,_height, colorPalette){
  override def getVal(ix: Double, iy: Double, x_offset: Double, y_offset: Double, scale: Double, max_iters: Int): (Complex, Int) = {
    var z = new Complex(((ix+x_offset) * 3 - 1.5 )/scale, ((iy+ y_offset) * 3 - 1.5 )/scale)
    var iters = 0
    // var c = new Complex(-0.8,0.156)

    while (z.radius < 10 && iters < max_iters) {
      z = function.apply(z)
      iters += 1
    }
    (z, iters)
  }
}
