package Model

/**
  * Created by dagum on 08-12-2017.
  */
trait ColorPalette {
  /***
    * Obtain color on a point with params
    * @param i used iters
    * @param iters max number of iterations
    * @param c point to obtain color
    * @return Tuple3 of rgb color [0-255]
    */
  def get(i: Int, iters: Int, c: Complex) : (Int, Int, Int)
}
