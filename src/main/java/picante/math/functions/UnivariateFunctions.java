package picante.math.functions;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;
import static picante.math.PicanteMath.max;
import static picante.math.PicanteMath.min;
import static picante.math.PicanteMath.pow;
import com.google.common.primitives.Doubles;
import picante.exceptions.RuntimeInterruptedException;
import picante.math.PicanteMath;

/**
 * Class of static utility methods for manipulating {@link UnivariateFunction}s.
 */
public class UnivariateFunctions {

  /**
   * Constructor is private to block instantiations.
   */
  private UnivariateFunctions() {}

  /**
   * Creates a new, constant function
   * 
   * @param constant the constant to return for all time.
   * 
   * @return constant
   */
  public static UnivariateFunction create(final double constant) {
    return new UnivariateFunction() {

      @Override
      public double evaluate(@SuppressWarnings("unused") double t) {
        return constant;
      }
    };
  }

  /**
   * Creates a new function that adds two functions together.
   * 
   * @param a a univariate function
   * @param b another univariate function
   * 
   * @return a newly created univariate function that computes the sum <code>( a + b )</code>.
   */
  public static UnivariateFunction add(final UnivariateFunction a, final UnivariateFunction b) {
    return new UnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return a.evaluate(t) + b.evaluate(t);
      }
    };
  }

  /**
   * Creates a new function that subtracts the right function from the left.
   * 
   * @param a a univariate function, the minuend
   * @param b another univariate function, the subtrahend
   * 
   * @return a newly created function that computes the difference <code>( a - b )</code>
   */
  public static UnivariateFunction subtract(final UnivariateFunction a,
      final UnivariateFunction b) {
    return new UnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return a.evaluate(t) - b.evaluate(t);
      }
    };
  }

  /**
   * Creates a new function that computes the product of two functions.
   * 
   * @param a a univariate function
   * @param b another univariate function
   * 
   * @return a newly created function that computes the product <code>( a * b )</code>.
   */
  public static UnivariateFunction multiply(final UnivariateFunction a,
      final UnivariateFunction b) {
    return new UnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return a.evaluate(t) * b.evaluate(t);
      }
    };
  }

  /**
   * Creates a new function that computes the quotient of two functions.
   * 
   * @param a a univariate function, the dividend
   * @param b another univariate function, the divisor
   * 
   * @return a newly created function that computes the quotient <code>( a / b )</code>
   * 
   */
  public static UnivariateFunction divide(final UnivariateFunction a, final UnivariateFunction b) {
    return new UnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return a.evaluate(t) / b.evaluate(t);
      }
    };
  }

  /**
   * Creates a new function that computes the composition of two functions.
   * 
   * @param a the outer function in the composition, evaluated last
   * @param b the inner function in the composition, evaluated first
   * 
   * @return a newly created function that computes the composition <code>a ( b ( t ) )</code>
   */
  public static UnivariateFunction compose(final UnivariateFunction a, final UnivariateFunction b) {
    return new UnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return a.evaluate(b.evaluate(t));
      }
    };
  }

  /**
   * Creates a new function that scales the supplied function by a constant.
   * 
   * @param scale the scale factor to apply to the function
   * 
   * @param function the function to scale
   * 
   * @return a newly created function that computes the product <code>scale * ( a )</code>
   */
  public static UnivariateFunction scale(final double scale, final UnivariateFunction function) {
    return new UnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return scale * function.evaluate(t);
      }
    };
  }

  /**
   * Creates a new function that negates the supplied function.
   * 
   * @param function the function to negate
   * 
   * @return a newly created function that computes <code>-(a)</code>
   */
  public static UnivariateFunction negate(final UnivariateFunction function) {
    return scale(-1.0, function);
  }

  /**
   * Simple interruptible delegate implementation
   */
  private static class InterruptibleFunctionDelegate implements UnivariateFunction {

    private final RuntimeInterruptionChecker checker;
    private final UnivariateFunction delegate;

    InterruptibleFunctionDelegate(RuntimeInterruptionChecker checker, UnivariateFunction delegate) {
      super();
      this.checker = checker;
      this.delegate = delegate;
    }

    @Override
    public double evaluate(double t) {
      checker.processRuntimeInterruptCheckAndThrow();
      return delegate.evaluate(t);
    }

  }

  /**
   * Wraps an existing function and provides a variant that will respond to
   * {@link Thread#interrupt()} by throwing {@link RuntimeInterruptedException} on interrupt.
   * 
   * @param function the function to wrap into an interruptible one
   * @param evaluationCheckModulo the total number of function evaluations to execute before
   *        checking {@link Thread#interrupted()}
   * 
   * @return the interruptible function
   */
  public static UnivariateFunction interruptibleFunction(UnivariateFunction function,
      int evaluationCheckModulo) {
    return new InterruptibleFunctionDelegate(
        RuntimeInterruptionChecker.createWithCheckModulo(evaluationCheckModulo), function);
  }

  /**
   * Wraps an existing function and provides a variant that will respond to
   * {@link Thread#interrupt()} by throwing {@link RuntimeInterruptedException} on interrupt.
   * <p>
   * This variant is similar to
   * {@link UnivariateFunctions#interruptibleFunction(UnivariateFunction, int)} except it checks
   * {@link Thread#interrupted()} every call into {@link UnivariateFunction#evaluate(double)}
   * </p>
   *
   * @param function the function to wrap
   * 
   * @return an interruptible function
   */
  public static UnivariateFunction interruptibleFunction(UnivariateFunction function) {
    return new InterruptibleFunctionDelegate(RuntimeInterruptionChecker.create(), function);
  }

  /**
   * Creates a new function that confines the values returned to the interval specified.
   * 
   * @param lowValue
   * @param highValue
   * @param function
   * 
   * @return
   * 
   * @throws IllegalArgumentException if lowValue >= highValue.
   */
  public static UnivariateFunction clamp(final double lowValue, final double highValue,
      final UnivariateFunction function) {
    checkNotNull(function);
    checkArgument(Doubles.isFinite(lowValue));
    checkArgument(Doubles.isFinite(highValue));
    checkArgument(lowValue < highValue,
        "Specified low value: %s must be strictly less than high value: %s", lowValue, highValue);

    return new UnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return max(lowValue, min(highValue, function.evaluate(t)));
      }
    };
  }

  public static UnivariateFunction clampHigh(final double highValue,
      final UnivariateFunction function) {
    checkNotNull(function);
    checkArgument(Doubles.isFinite(highValue));
    return new UnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return min(highValue, function.evaluate(t));
      }
    };

  }

  public static UnivariateFunction clampLow(final double lowValue,
      final UnivariateFunction function) {
    checkNotNull(function);
    checkArgument(Doubles.isFinite(lowValue));
    return new UnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return max(lowValue, function.evaluate(t));
      }
    };
  }

  /**
   * Uninitialized field that retains the {@link PicanteMath#toDegrees(double)} wrapper function.
   */
  private static final UnivariateFunction MATH_TO_DEGREES = new UnivariateFunction() {

    @Override
    public double evaluate(double t) {
      return PicanteMath.toDegrees(t);
    }
  };

  /**
   * Returns a {@link UnivariateFunction} that simply invokes
   * {@link PicanteMath#toDegrees(double)}.
   * 
   * @return function that converts radians to degrees
   */
  public static UnivariateFunction toDegrees() {
    return MATH_TO_DEGREES;
  }

  /**
   * Uninitialized field that retains the {@link PicanteMath#toRadians(double)} wrapper function.
   */
  private static final UnivariateFunction MATH_TO_RADIANS = new UnivariateFunction() {

    @Override
    public double evaluate(double t) {
      return PicanteMath.toRadians(t);
    }
  };

  /**
   * Returns a {@link UnivariateFunction} that simply invokes
   * {@link PicanteMath#toRadians(double)}.
   * 
   * @return function that converts degrees to radians
   */
  public static UnivariateFunction toRadians() {
    return MATH_TO_RADIANS;
  }

  /**
   * Uninitialized field that retains the {@link PicanteMath#atan(double)} wrapper function.
   */
  private static final UnivariateFunction MATH_ATAN = new UnivariateFunction() {

    @Override
    public double evaluate(double t) {
      return PicanteMath.atan(t);
    }
  };

  /**
   * Returns a {@link UnivariateFunction} that simply invokes {@link PicanteMath#atan(double)}.
   * 
   * @return function that computes arc tangent
   */
  public static UnivariateFunction atan() {
    return MATH_ATAN;
  }

  /**
   * Uninitialized field that retains the {@link PicanteMath#cos(double)} wrapper function.
   */
  private static final UnivariateFunction MATH_COS = new UnivariateFunction() {

    @Override
    public double evaluate(double t) {
      return PicanteMath.cos(t);
    }
  };

  /**
   * Returns a {@link UnivariateFunction} that simply invokes {@link PicanteMath#cos(double)}.
   * 
   * @return function that computes the cosine of the argument in radians
   */
  public static UnivariateFunction cos() {
    return MATH_COS;
  }

  /**
   * Uninitialized field that retains the {@link PicanteMath#sin(double)} wrapper function.
   */
  private static final UnivariateFunction MATH_SIN = new UnivariateFunction() {

    @Override
    public double evaluate(double t) {
      return PicanteMath.sin(t);
    }
  };

  /**
   * Returns a {@link UnivariateFunction} that simply invokes {@link PicanteMath#sin(double)}.
   * 
   * @return function that computes the sine of the argument in radians
   */
  public static UnivariateFunction sin() {
    return MATH_SIN;
  }

  /**
   * Uninitialized field that retains the {@link PicanteMath#sqrt(double)} wrapper function.
   */
  private static final UnivariateFunction MATH_SQRT = new UnivariateFunction() {

    @Override
    public double evaluate(double t) {
      return PicanteMath.sqrt(t);
    }
  };

  /**
   * Returns a {@link UnivariateFunction} that simply invokes {@link PicanteMath#sqrt(double)}.
   * 
   * @return function that computes the square root of the argument
   */
  public static UnivariateFunction sqrt() {
    return MATH_SQRT;
  }

  /**
   * Uninitialized field that retains the {@link PicanteMath#log10(double)} wrapper function.
   */
  private static final UnivariateFunction MATH_LOG10 = new UnivariateFunction() {

    @Override
    public double evaluate(double t) {
      return PicanteMath.log10(t);
    }
  };

  /**
   * Returns a {@link UnivariateFunction} that simply invokes {@link PicanteMath#log10(double)}.
   * 
   * @return function that computes the base 10 logarithm of the argument
   */
  public static UnivariateFunction log10() {
    return MATH_LOG10;
  }

  /**
   * Uninitialized field that retains the pow10(double) wrapper function.
   */
  private static final UnivariateFunction MATH_POW10 = new UnivariateFunction() {
    @Override
    public double evaluate(double t) {
      return pow(10, t);
    }
  };

  /**
   * Returns a {@link UnivariateFunction} that simply invokes Math.pow(10,x)
   * 
   * @return function that computes the base 10 exponential of the argument
   */
  public static UnivariateFunction pow10() {
    return MATH_POW10;
  }

  /**
   * Returns a {@link UnivariateFunction} that just returns the argument
   * 
   * @return identity function: f(x) = x
   */
  public static UnivariateFunction identity() {
    return new UnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return t;
      }
    };
  }

}
