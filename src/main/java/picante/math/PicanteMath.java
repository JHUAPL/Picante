package picante.math;

import com.google.common.annotations.VisibleForTesting;
import net.jafama.FastMath;

/**
 * This defines the standard set of Math methods to be used throughout the Picante library. This
 * class was created because there are 3rd party implementations of standard math methods that are
 * much faster compared to the standard JDK (java.lang.Math) methods.
 * <p>
 * As of now, the implementation that we have chosen in
 * <a href="https://github.com/jeffhain/jafama">JAFAMA</a>
 * <p>
 * I took the method comments from {@link FastMath}.
 * 
 * @author G.K.Stephens
 *
 */
public class PicanteMath {

  /*
   * Switches to the JDK (java.lang.Math) instead. As this should be only be set to true once, and
   * because making this lockable would have performance implications, I choose to make this
   * volatile instead of using AtomicBoolean.
   */
  private static volatile boolean USE_JDK_MATH = false;

  /**
   * Constructor should be private
   */
  private PicanteMath() {};

  /**
   * Calling this method uses the standard JDK (java.lang.Math) methods instead of the preferred
   * Crucible math methods. This is intended to be called once at the beginning of the application.
   * Therefore, and because of performance considerations, this has not been implemented in a thread
   * safe way. To encourage safe use with this method, no method has been provided to switch back.
   */
  public static void useJdkMath() {
    USE_JDK_MATH = true;
  }

  /**
   * This shouldn't be called, it is only here for unit testing.
   */
  @VisibleForTesting
  static void useFastMath() {
    USE_JDK_MATH = false;
  }

  /**
   * The {@code double} value that is closer than any other to <i>e</i>, the base of the natural
   * logarithms.
   */
  public static final double E = Math.E;

  /**
   * The {@code double} value that is closer than any other to <i>pi</i>, the ratio of the
   * circumference of a circle to its diameter.
   */
  public static final double PI = Math.PI;

  /**
   * @param angle Angle in radians.
   * @return Angle sine.
   */
  public static double sin(double a) {
    if (USE_JDK_MATH) {
      return Math.sin(a);
    }
    return FastMath.sin(a);
  }

  /**
   * @param angle Angle in radians.
   * @return Angle cosine.
   */
  public static double cos(double a) {
    if (USE_JDK_MATH) {
      return Math.cos(a);
    }
    return FastMath.cos(a);
  }

  /**
   * Can have very bad relative error near +-PI/2, but of the same magnitude than the relative delta
   * between StrictMath.tan(PI/2) and StrictMath.tan(nextDown(PI/2)).
   * 
   * @param angle Angle in radians.
   * @return Angle tangent.
   */
  public static double tan(double a) {
    if (USE_JDK_MATH) {
      return Math.tan(a);
    }
    return FastMath.tan(a);
  }

  /**
   * @param value Value in [-1,1].
   * @return Value arcsine, in radians, in [-PI/2,PI/2].
   */
  public static double asin(double a) {
    if (USE_JDK_MATH) {
      return Math.asin(a);
    }
    return FastMath.asin(a);
  }

  /**
   * @param value Value in [-1,1].
   * @return Value arccosine, in radians, in [0,PI].
   */
  public static double acos(double a) {
    if (USE_JDK_MATH) {
      return Math.acos(a);
    }
    return FastMath.acos(a);
  }

  /**
   * @param value A double value.
   * @return Value arctangent, in radians, in [-PI/2,PI/2].
   */
  public static double atan(double a) {
    if (USE_JDK_MATH) {
      return Math.atan(a);
    }
    return FastMath.atan(a);
  }

  /**
   * Gives same result as Math.toRadians for some particular values like 90.0, 180.0 or 360.0, but
   * is faster (no division).
   * 
   * @param angdeg Angle value in degrees.
   * @return Angle value in radians.
   */
  public static double toRadians(double angdeg) {
    if (USE_JDK_MATH) {
      return Math.toRadians(angdeg);
    }
    return FastMath.toRadians(angdeg);
  }

  /**
   * Gives same result as Math.toDegrees for some particular values like Math.PI/2, Math.PI or
   * 2*Math.PI, but is faster (no division).
   * 
   * @param angrad Angle value in radians.
   * @return Angle value in degrees.
   */
  public static double toDegrees(double angrad) {
    if (USE_JDK_MATH) {
      return Math.toDegrees(angrad);
    }
    return FastMath.toDegrees(angrad);
  }

  /**
   * @param value A double value.
   * @return e^value.
   */
  public static double exp(double a) {
    if (USE_JDK_MATH) {
      return Math.exp(a);
    }
    return FastMath.exp(a);
  }

  /**
   * @param value A double value.
   * @return Value logarithm (base e).
   */
  public static double log(double a) {
    if (USE_JDK_MATH) {
      return Math.log(a);
    }
    return FastMath.log(a);
  }

  /**
   * @param value A double value.
   * @return Value logarithm (base 10).
   */
  public static double log10(double a) {
    if (USE_JDK_MATH) {
      return Math.log10(a);
    }
    return FastMath.log10(a);
  }

  /**
   * @param value A double value.
   * @return 1+Value logarithm (base e).
   */
  public static double log1p(double a) {
    if (USE_JDK_MATH) {
      return Math.log1p(a);
    }
    return FastMath.log1p(a);
  }

  /**
   * @param value A double value.
   * @return Value square root.
   */
  public static double sqrt(double a) {
    if (USE_JDK_MATH) {
      return Math.sqrt(a);
    }
    return FastMath.sqrt(a);
  }

  /**
   * @param value A double value.
   * @return Value cubic root.
   */
  public static double cbrt(double a) {
    if (USE_JDK_MATH) {
      return Math.cbrt(a);
    }
    return FastMath.cbrt(a);
  }

  public static double IEEEremainder(double f1, double f2) {
    if (USE_JDK_MATH) {
      return Math.IEEEremainder(f1, f2);
    }
    return FastMath.IEEEremainder(f1, f2);
  }

  /**
   * @param value A double value.
   * @return Ceiling of value.
   */
  public static double ceil(double a) {
    if (USE_JDK_MATH) {
      return Math.ceil(a);
    }
    return FastMath.ceil(a);
  }

  /**
   * @param value A double value.
   * @return Floor of value.
   */
  public static double floor(double a) {
    if (USE_JDK_MATH) {
      return Math.floor(a);
    }
    return FastMath.floor(a);
  }

  /**
   * For special values for which multiple conventions could be adopted, behaves like
   * Math.atan2(double,double).
   * 
   * @param y Coordinate on y axis.
   * @param x Coordinate on x axis.
   * @return Angle from x axis positive side to (x,y) position, in radians, in [-PI,PI]. Angle
   *         measure is positive when going from x axis to y axis (positive sides).
   */
  public static double atan2(double y, double x) {
    if (USE_JDK_MATH) {
      return Math.atan2(y, x);
    }
    return FastMath.atan2(y, x);
  }

  /**
   * 1e-13ish accuracy or better on whole double range.
   * 
   * @param value A double value.
   * @param power A power.
   * @return value^power.
   */
  public static double pow(double a, double b) {
    if (USE_JDK_MATH) {
      return Math.pow(a, b);
    }
    return FastMath.pow(a, b);
  }

  /**
   * Might have different semantics than Math.round(double), see bugs 6430675 and 8010430.
   * 
   * @param value A double value.
   * @return Value rounded to nearest long, choosing superior long in case two are equally close
   *         (i.e. rounding-up).
   */
  public static long round(double a) {
    if (USE_JDK_MATH) {
      return Math.round(a);
    }
    return FastMath.round(a);
  }

  /**
   * @param value An int value.
   * @return The absolute value, except if value is Integer.MIN_VALUE, for which it returns
   *         Integer.MIN_VALUE.
   */
  public static int abs(int a) {
    return Math.abs(a);
  }

  /**
   * Returns the absolute value of a {@code double} value. If the argument is not negative, the
   * argument is returned. If the argument is negative, the negation of the argument is returned.
   * Special cases:
   * <ul>
   * <li>If the argument is positive zero or negative zero, the result is positive zero.
   * <li>If the argument is infinite, the result is positive infinity.
   * <li>If the argument is NaN, the result is NaN.
   * </ul>
   * In other words, the result is the same as the value of the expression:
   * <p>
   * {@code Double.longBitsToDouble((Double.doubleToLongBits(a)<<1)>>>1)}
   *
   * @param a the argument whose absolute value is to be determined
   * @return the absolute value of the argument.
   */
  public static double abs(double a) {
    if (USE_JDK_MATH) {
      return Math.abs(a);
    }
    return FastMath.abs(a);
  }

  /**
   * Returns the greater of two {@code int} values. That is, the result is the argument closer to
   * the value of {@link Integer#MAX_VALUE}. If the arguments have the same value, the result is
   * that same value.
   *
   * @param a an argument.
   * @param b another argument.
   * @return the larger of {@code a} and {@code b}.
   */
  public static int max(int a, int b) {
    return Math.max(a, b);
  }

  /**
   * Returns the greater of two {@code double} values. That is, the result is the argument closer to
   * positive infinity. If the arguments have the same value, the result is that same value. If
   * either value is NaN, then the result is NaN. Unlike the numerical comparison operators, this
   * method considers negative zero to be strictly smaller than positive zero. If one argument is
   * positive zero and the other negative zero, the result is positive zero.
   *
   * @param a an argument.
   * @param b another argument.
   * @return the larger of {@code a} and {@code b}.
   */
  public static double max(double a, double b) {
    if (USE_JDK_MATH) {
      return Math.max(a, b);
    }
    return FastMath.max(a, b);
  }

  /**
   * Returns the smaller of two {@code int} values. That is, the result the argument closer to the
   * value of {@link Integer#MIN_VALUE}. If the arguments have the same value, the result is that
   * same value.
   *
   * @param a an argument.
   * @param b another argument.
   * @return the smaller of {@code a} and {@code b}.
   */
  public static int min(int a, int b) {
    return Math.min(a, b);
  }

  /**
   * Returns the smaller of two {@code double} values. That is, the result is the value closer to
   * negative infinity. If the arguments have the same value, the result is that same value. If
   * either value is NaN, then the result is NaN. Unlike the numerical comparison operators, this
   * method considers negative zero to be strictly smaller than positive zero. If one argument is
   * positive zero and the other is negative zero, the result is negative zero.
   *
   * @param a an argument.
   * @param b another argument.
   * @return the smaller of {@code a} and {@code b}.
   */
  public static double min(double a, double b) {
    if (USE_JDK_MATH) {
      return Math.min(a, b);
    }
    return FastMath.min(a, b);
  }

  /**
   * The ULP (Unit in the Last Place) is the distance to the next value larger in magnitude.
   *
   * @param value A double value.
   * @return The size of an ulp of the specified value, or Double.MIN_VALUE if it is +-0.0, or
   *         +Infinity if it is +-Infinity, or NaN if it is NaN.
   */
  public static double ulp(double d) {
    if (USE_JDK_MATH) {
      return Math.ulp(d);
    }
    return FastMath.ulp(d);
  }

  /**
   * @param value A double value.
   * @return -1.0 if the specified value is < 0, 1.0 if it is > 0, and the value itself if it is NaN
   *         or +-0.0.
   */
  public static double signum(double d) {
    if (USE_JDK_MATH) {
      return Math.signum(d);
    }
    return FastMath.signum(d);
  }

  /**
   * Some properties of sinh(x) = (exp(x)-exp(-x))/2:
   * 
   * <pre>
   * 1) defined on ]-Infinity,+Infinity[
   * 2) result in ]-Infinity,+Infinity[
   * 3) sinh(x) = -sinh(-x) (implies sinh(0) = 0)
   * 4) sinh(epsilon) ~= epsilon
   * 5) lim(sinh(x),x->+Infinity) = +Infinity
   *    (y increasing exponentially faster than x)
   * 6) reaches +Infinity (double overflow) for x >= 710.475860073944,
   *    i.e. a bit further than exp(x)
   * </pre>
   * 
   * @param x A double value.
   * @return Value hyperbolic sine.
   */
  public static double sinh(double x) {
    if (USE_JDK_MATH) {
      return Math.sinh(x);
    }
    return FastMath.sinh(x);
  }

  /**
   * Some properties of cosh(x) = (exp(x)+exp(-x))/2:
   * 
   * <pre>
   * 1) defined on ]-Infinity,+Infinity[
   * 2) result in [1,+Infinity[
   * 3) cosh(0) = 1
   * 4) cosh(x) = cosh(-x)
   * 5) lim(cosh(x),x->+Infinity) = +Infinity
   *    (y increasing exponentially faster than x)
   * 6) reaches +Infinity (double overflow) for x >= 710.475860073944,
   *    i.e. a bit further than exp(x)
   * </pre>
   * 
   * @param x A double value.
   * @return Value hyperbolic cosine.
   */
  public static double cosh(double x) {
    if (USE_JDK_MATH) {
      return Math.cosh(x);
    }
    return FastMath.cosh(x);
  }

  /**
   * Some properties of tanh(x) = sinh(x)/cosh(x) = (exp(2*x)-1)/(exp(2*x)+1):
   * 
   * <pre>
   * 1) defined on ]-Infinity,+Infinity[
   * 2) result in ]-1,1[
   * 3) tanh(x) = -tanh(-x) (implies tanh(0) = 0)
   * 4) tanh(epsilon) ~= epsilon
   * 5) lim(tanh(x),x->+Infinity) = 1
   * 6) reaches 1 (double loss of precision) for x = 19.061547465398498
   * </pre>
   * 
   * @param x A double value.
   * @return Value hyperbolic tangent.
   */
  public static double tanh(double x) {
    if (USE_JDK_MATH) {
      return Math.tanh(x);
    }
    return FastMath.tanh(x);
  }

  /**
   * Some properties of acosh(x) = log(x + sqrt(x^2 - 1)):
   * 
   * <pre>
   * 1) defined on [1,+Infinity[
   * 2) result in ]0,+Infinity[ (by convention, since cosh(x) = cosh(-x))
   * 3) acosh(1) = 0
   * 4) acosh(1+epsilon) ~= log(1 + sqrt(2*epsilon)) ~= sqrt(2*epsilon)
   * 5) lim(acosh(x),x->+Infinity) = +Infinity
   *    (y increasing logarithmically slower than x)
   * </pre>
   * 
   * @param x A double value.
   * @return Value hyperbolic arccosine.
   */
  public static double acosh(double x) {
    return FastMath.acosh(x);
  }

  /**
   * Some properties of asinh(x) = log(x + sqrt(x^2 + 1)):
   * 
   * <pre>
   * 1) defined on ]-Infinity,+Infinity[
   * 2) result in ]-Infinity,+Infinity[
   * 3) asinh(x) = -asinh(-x) (implies asinh(0) = 0)
   * 4) asinh(epsilon) ~= epsilon
   * 5) lim(asinh(x),x->+Infinity) = +Infinity
   *    (y increasing logarithmically slower than x)
   * </pre>
   * 
   * @param x A double value.
   * @return Value hyperbolic arcsine.
   */
  public static double asinh(double x) {
    return FastMath.asinh(x);
  }

  /**
   * Some properties of atanh(x) = log((1+x)/(1-x))/2:
   * 
   * <pre>
   * 1) defined on ]-1,1[
   * 2) result in ]-Infinity,+Infinity[
   * 3) atanh(-1) = -Infinity (by continuity)
   * 4) atanh(1) = +Infinity (by continuity)
   * 5) atanh(epsilon) ~= epsilon
   * 6) lim(atanh(x),x->1) = +Infinity
   * </pre>
   * 
   * @param x A double value.
   * @return Value hyperbolic arctangent.
   */
  public static double atanh(double x) {
    return FastMath.atanh(x);
  }

  /**
   * @return sqrt(x^2+y^2) without intermediate overflow or underflow.
   */
  public static double hypot(double x, double y) {
    if (USE_JDK_MATH) {
      return Math.hypot(x, y);
    }
    return FastMath.hypot(x, y);
  }

  /**
   * @param x A double value.
   * @return The double mathematical integer closest to the specified value, choosing even one if
   *         two are equally close, or respectively NaN, +-Infinity or +-0.0 if the value is any of
   *         these.
   */
  public static double rint(double x) {
    if (USE_JDK_MATH) {
      return Math.rint(x);
    }
    return FastMath.rint(x);

  }

}
