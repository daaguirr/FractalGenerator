package Model

/**
  * Created by dagum on 08-12-2017.
  * Palette with predominant blue colors
  */
class ColdPalette extends ColorPalette{
  override def get(i: Int, iters: Int, c: Complex): (Int, Int, Int) = {
    if (c.radius > 2) {
      val t = i * 1.0 / iters
      val r = (9 * (1 - t) * t * t * t * 255).toInt
      val g = (15 * (1 - t) * (1 - t) * t * t * 255).toInt
      val b = (8.5 * (1 - t) * (1 - t) * (1 - t) * t * 255).toInt
      (r, g, b)
    }
    else (0, 0, 0)
  }
}
