package Model

import com.opengamma.strata.math.impl.interpolation.PiecewisePolynomialResult
import com.opengamma.strata.math.impl.function.PiecewisePolynomialFunction1D
import com.opengamma.strata.math.impl.interpolation.PiecewiseCubicHermiteSplineInterpolator

/**
  * Created by dagum on 08-12-2017.
  * Palette with common fractal colors
  * Idea: Interpolate with spline interpolation 5 points representing colors con a gradient
  *       then create a list of tuple3 representing colors with equal spacing partition of [0,1] on 512 parts
  */
class PrettyPalette extends ColorPalette{
  // Interpolation Tools
  private val PCHIP = new PiecewiseCubicHermiteSplineInterpolator
  private val PPVAL = new PiecewisePolynomialFunction1D

  // Initial raw values
  private val one_over_log2 = 1.0 / math.log(2.0) // Magic constant
  private val x : Array[Double] = Array(0.0, 0.16, 0.42, 0.6425, 0.8575, 0.99) // color gradient position
  private val r_raw : Array[Double] = Array(0, 32, 237, 255, 0, 0) // red channel points
  private val g_raw : Array[Double] = Array(7, 107, 255, 170, 2, 7) // green channel point
  private val b_raw : Array[Double] = Array(100, 203, 255, 0, 0, 95) // blue channel point

  private val lsp = new Array[Double](512) // partition of [0,1]
  for(i <- 0 until 512) lsp(i) = i/512.0

  val r: PiecewisePolynomialResult = PCHIP.interpolate(x,r_raw)
  val g: PiecewisePolynomialResult = PCHIP.interpolate(x,g_raw)
  val b: PiecewisePolynomialResult = PCHIP.interpolate(x,b_raw)

  // result
  val gradient: Array[(Int, Int, Int)] = lsp.map(a => (PPVAL.evaluate(r,a).get(0).toInt % 256,PPVAL.evaluate(g,a).get(0).toInt % 256,PPVAL.evaluate(b,a).get(0).toInt % 256))


  override def get(i: Int, iters: Int, c: Complex): (Int, Int, Int) = {
    val size = c.radius
    val smoothed = math.log(math.log(size) * one_over_log2) * one_over_log2

    // border cases
    if (math.log(size) < 0 || i + 1 < smoothed) return (0, 0, 0)
    if (size == 1) return (0, 0, 0)
    if (smoothed.isNaN || smoothed.isNegInfinity || smoothed.isInfinity || i+1 < smoothed) return (0,0,0)

    val colorI = (math.sqrt(i + 1 - smoothed) * 256).toInt % gradient.length
    gradient(colorI)
  }
}
