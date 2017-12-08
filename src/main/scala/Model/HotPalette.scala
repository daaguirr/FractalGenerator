package Model

/**
  * Created by dagum on 08-12-2017.
  */
class HotPalette extends ColorPalette {
  override def get(i: Int, iters: Int, c: Complex): (Int, Int, Int) = {
    if (c.radius > 2) {
      val v = 765 * i / iters
      if (v > 510) (255, 255, v % 255)
      else if (v > 255) (255, v % 255, 0)
      else (v % 255, 0, 0)
    }
    else (0, 0, 0)
  }
}
