package Model


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author John Miller
  * @version 1.0
  * @see LICENSE (MIT style license file).
  */

import scala.language.implicitConversions

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DoubleWithExp` class defines an expontiation operator '~' for Doubles.
  *
  * @param x the base
  */
case class DoubleWithExp(x: Double) {
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Exponentiation operator for scala Doubles (x ~ y).
    *
    * @param y the exponent
    */
  def ~^(y: Double): Double = math.pow(x, y)

} // DoubleWithExp class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DoubleWithExp` companion object provides implicit conversion from Double
  * to DoubleWithExp allowing '~' to be applied to Doubles.
  * It also provide a negative exponential function.
  *
  */
object DoubleWithExp {
  implicit def doubleWithExp(d: Double): DoubleWithExp = DoubleWithExp(d)

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Negative exponential funtion (e to the minus x).
    *
    * @param x the argument of the function
    */
  def nexp(x: Double): Double = math.exp(-x)

} // DoubleWithExp object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DoubleWithExpTest` object is used to test the `DoubleWithExp` class.
  */
object DoubleWithExpTest extends App {

  import DoubleWithExp._

  println(2.0 ~^ 3.0)

} // DoubleWithExpTest object

import Model.DoubleWithExp._

import scala.math.{acos, cos, signum, sin}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to represent complex numbers (a + bi) as (a, b), e.g.,
  * (2.1, 3.2i).  Note: i * i = -1.
  *
  * @param re the real part
  * @param im the imaginary part
  */
case class Complex(re: Double, im: Double = 0.0)
  extends Fractional[Complex] with Ordered[Complex] {
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Compute the unary minus (-).
    */
  def unary_-() = Complex(-re, -im)

  def negate(c: Complex): Complex = -c

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Add two complex numbers.
    *
    * @param c add complex c to this
    */
  def +(c: Complex) = Complex(re + c.re, im + c.im)

  def plus(c: Complex, d: Complex): Complex = c + d

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Add a complex number plus double.
    *
    * @param r add r to this
    */
  def +(r: Double) = Complex(re + r, im)

  def plus(c: Complex, r: Double): Complex = c + r

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Subtract two complex numbers.
    *
    * @param c subtract c from this
    */
  def -(c: Complex) = Complex(re - c.re, im - c.im)

  def minus(c: Complex, d: Complex): Complex = c - d

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Subtract: a complex number minus double.
    *
    * @param r subtract r from this
    */
  def -(r: Double) = Complex(re - r, im)

  def minus(c: Complex, r: Double): Complex = c - r

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Multiply two complex numbers.
    *
    * @param c multiply this times c
    */
  def *(c: Complex) = Complex(re * c.re - im * c.im, re * c.im + im * c.re)

  def times(c: Complex, d: Complex): Complex = c * d

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Multiply a complex numbers times double.
    *
    * @param r multiply this times r
    */
  def *(r: Double) = Complex(re * r, im * r)

  def times(c: Complex, r: Double): Complex = c * r

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Divide two complex numbers.
    *
    * @param c divide this by c
    */
  def /(c: Complex) = Complex((re * c.re + im * c.im) / (c.re * c.re + c.im * c.im),
    (im * c.re - re * c.im) / (c.re * c.re + c.im * c.im))

  def div(c: Complex, d: Complex): Complex = c / d

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Divide a complex numbers div double.
    *
    * @param r divide this by r
    */
  def /(r: Double) = Complex((re * r) / (r * r), (im * r) / (r * r))

  def div(c: Complex, r: Double): Complex = c / r

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Raise a complex to the r-th power (a double) using polar coordinates.
    *
    * @param r the power/exponent
    */
  def ~^(r: Double): Complex = {
    val (rad, ang) = polar
    Complex.create(rad ~^ r, ang * r)
  }

  def pow(c: Complex, r: Double): Complex = c ~^ r

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the radius of the complex number as a vector in the re-im plane.
    */
  def radius: Double = math.sqrt(re ~^ 2.0+ im~^2.0)

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the angle of the complex number as a vector in the re-im plane.
    */
  def angle: Double = acos(re / radius)

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the complex number in polar coordinates (radius, angle).
    */
  def polar: (Double, Double) = {
    val rad = radius
    (rad, acos(re / rad))
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the complex conjugate: if z = (a + bi) then z.bar = (a - bi).
    */
  def bar = Complex(re, -im)

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the absolute value of this complex number.
    */
  def abs = Complex(re.abs, im.abs)

  def log =
    new Complex(math.log(radius), math.atan2(im, re))

  def exp: Complex = {
    val expreal = math.exp(re)
    Complex(expreal * math.cos(im), expreal * math.sin(im))
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the maximum of this and that complex numbers.
    *
    * @param c that complex number to compare with this
    */
  def max(c: Complex): Complex = if (c > this) c else this

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the minimum of this and that complex numbers.
    *
    * @param c that complex number to compare with this
    */
  def min(c: Complex): Complex = if (c < this) c else this

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Determine whether this complex number is real (no imaginary part).
    */
  def isRe: Boolean = im == 0.0

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Compare two complex numbers (negative for <, zero for ==, positive for >).
    *
    * @param c the first complex number to compare
    * @param d the second complex number to compare
    */
  def compare(c: Complex, d: Complex): Int = {
    if (c.re == d.re) c.im compare d.im else c.re compare d.re
  } // compare

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Compare this complex number with that complex number d.
    *
    * @param d that complex number
    */
  def compare(d: Complex): Int = {
    if (re == d.re) im compare d.im else re compare d.re
  } // compare

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Convert that/this complex number to a Double.
    *
    * @param c that complex number to convert
    */
  def toDouble(c: Complex): Double = c.re

  def toDouble: Double = re

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Convert that/this complex number to a Float.
    *
    * @param c that complex number to convert
    */
  def toFloat(c: Complex): Float = c.re.toFloat

  def toFloat: Float = re.toFloat

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Convert that/this complex number to an Int.
    *
    * @param c that complex number to convert
    */
  def toInt(c: Complex): Int = c.re.toInt

  def toInt: Int = re.toInt

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Convert this complex number to a Long.
    *
    * @param c that complex number to convert
    */
  def toLong(c: Complex): Long = c.re.toLong

  def toLong: Long = re.toLong

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Create a complex number from a Double.
    *
    * @param x the double used to create the complex number
    */
  def fromDouble(x: Double) = Complex(x)

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Create a complex number from a Float.
    *
    * @param x the float used to create the complex number
    */
  def fromFloat(x: Float) = Complex(x)

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Create a complex number from an Int.
    *
    * @param n the integer used to create the complex number
    */
  def fromInt(n: Int) = Complex(n)

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Create a complex number from a Long.
    *
    * @param n the long used to create the complex number
    */
  def fromLong(n: Long) = Complex(n)

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Override equals to determine whether this complex number equals complex c.
    *
    * @param c the complex number to compare with this
    */
  override def equals(c: Any): Boolean = {
    c.isInstanceOf[Complex] && (re equals c.asInstanceOf[Complex].re) &&
      (im equals c.asInstanceOf[Complex].im)
  } // equals

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Must also override hashCode to be be compatible with equals.
    */
  override def hashCode: Int = re.hashCode + 41 * im.hashCode

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Convert this complex number to a String.
    */
  override def toString: String = "Complex ( " + re + " , " + im + "i )"

} // Complex class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This companion object defines the origin (zero) and the fourth roots of unity
  * as well as some utility functions.
  */
object Complex {
  /** Zero (0) as a Complex number
    */
  val _0 = Complex(0.0)

  /** One (1) as a Complex number
    */
  val _1 = Complex(1.0)

  /** Imaginary one (i) as a Complex number
    */
  val _i = Complex(0.0, 1.0)

  /** Negative one (-1) as a Complex number
    */
  val _1n = Complex(-1.0)

  /** Negative imaginary one (-i) as a Complex number
    */
  val _in = Complex(0.0, -1.0)

  private val rr2 = 1.0 / math.sqrt(2.0) // reciprocal root of 2.

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Create a complex number from the given polar coordinates.
    *
    * @param rad the radius (the length of the vector in the re-im plane)
    * @param ang the angle (the angle of the vector above the re-axis)
    */
  def create(rad: Double, ang: Double): Complex = Complex(rad * cos(ang), rad * sin(ang))

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Compute an arthmetic results (+, -, *, /) where the first argument is
    * real and the second argument is complex.
    *
    *          def + (r: Double, c: Complex) = Complex (r + c.re, c.im)
    *          def - (r: Double, c: Complex) = Complex (r - c.re, c.im)
    *          def * (r: Double, c: Complex) = Complex (r * c.re, r * c.im)
    *          def / (r: Double, c: Complex) = Complex ((r * c.re) / (c.re * c.re + c.im * c.im),
    *          (-r * c.im) / (c.re * c.re + c.im * c.im))
    */

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the absolute value of that complex number.
    *
    * @param c that complex number
    */
  def abs(c: Complex): Complex = c.abs

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the square root of that complex number.
    *
    * @see www.mathpropress.com/stan/bibliography/complexSquareRoot.pdf
    * @param c that complex number
    */
  def sqrt(c: Complex): Complex = {
    val (a, b) = (c.re, c.im)
    val rad = c.radius
    Complex(rr2 * math.sqrt(rad + a),
      rr2 * math.sqrt(rad - a) * signum(b))
  } // sqrt

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Ordering for complex numbers.
    */
  val ord = new Ordering[Complex] {
    def compare(x: Complex, y: Complex): Int = x compare y
  }

} // Complex object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Complex class.
  */
object ComplexTest extends App {

  import Complex._

  import util.Sorting.quickSort

  val c = Complex(2.0, 3.0)
  val d = Complex(4.0, 5.0)
  val e = Complex(5.0)
  val r = sqrt(c)

  println("c       = " + c)
  println("d       = " + d)
  println("e       = " + e)
  println("-c      = " + -c)
  println("c + d   = " + (c + d))
  println("c - d   = " + (c - d))
  println("c * d   = " + (c * d))
  println("c / d   = " + (c / d))
  println("c ~^ 2. = " + (c ~^ 2.0))
  println("c * c   = " + (c * c))
  println("c.bar   = " + c.bar)
  println("c.abs   = " + c.abs)
  println("c max d = " + (c max d))
  println("c min d = " + (c min d))
  println("d.isRe  = " + d.isRe)
  println("e.isRe  = " + e.isRe)
  println("sqrt(c) = " + r)
  println("r * r   = " + (r * r))
  println("c < d   = " + (c < d))
  println("d < c   = " + (d < c))

  def sort(arr: Array[Complex]) {
    quickSort(arr)(Complex.ord)
  } // sort

  val arr = Array(e, d, c)
  println("arr = " + arr.deep)
  sort(arr)
  println("arr = " + arr.deep)

} // ComplexTest object
