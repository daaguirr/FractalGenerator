package Model

/**
  * Created by dagum on 08-12-2017.
  */
trait ColorPalette {
  def get(i: Int, iters: Int, c: Complex) : (Int, Int, Int)
}
