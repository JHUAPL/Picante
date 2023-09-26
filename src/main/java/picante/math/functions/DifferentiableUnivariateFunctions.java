package picante.math.functions;

import static picante.math.PicanteMath.cos;
import static picante.math.PicanteMath.sin;
import picante.exceptions.RuntimeInterruptedException;
import picante.math.PicanteMath;

/**
 * Class of static utility methods for manipulating {@link DifferentiableUnivariateFunction}s.
 * 
 * TODO: Consider reordering operations to preserve numerical precision.
 * 
 */
public class DifferentiableUnivariateFunctions {

  /**
   * Constructor is private to block instantiations.
   */
  private DifferentiableUnivariateFunctions() {}

  /**
   * Creates a constant valued, differentiable function.
   * 
   * @param constant the constant value to assume.
   * 
   * @return a newly created constant value, differentiable function.
   */
  public static DifferentiableUnivariateFunction create(final double constant) {
    return new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(@SuppressWarnings("unused") double t) {
        return constant;
      }

      @Override
      public double differentiate(@SuppressWarnings("unused") double t) {
        return 0;
      }
    };
  }

  /**
   * Creates a univariate function from the supplied differentiable one by capturing the derivative
   * 
   * @param function a differentiable function
   * 
   * @return a function that is the derivative of the supplied function
   */
  public static UnivariateFunction derivative(final DifferentiableUnivariateFunction function) {
    return new UnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return function.differentiate(t);
      }
    };
  }

  /**
   * Creates a new function that adds two functions together.
   * 
   * @param a a differentiable function
   * @param b another differentiable function
   * 
   * @return a newly created function that computes the sum <code>( a + b )</code>.
   */
  public static DifferentiableUnivariateFunction add(final DifferentiableUnivariateFunction a,
      final DifferentiableUnivariateFunction b) {
    return new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return a.evaluate(t) + b.evaluate(t);
      }

      @Override
      public double differentiate(double t) {
        return a.differentiate(t) + b.differentiate(t);
      }
    };
  }

  /**
   * Creates a new function that subtracts the right function from the left.
   * 
   * @param a a differentiable function, the minuend
   * @param b another differentiable function, the subtrahend
   * 
   * @return a newly created function that computes the difference <code>( a - b )</code>
   */
  public static DifferentiableUnivariateFunction subtract(final DifferentiableUnivariateFunction a,
      final DifferentiableUnivariateFunction b) {

    return new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return a.evaluate(t) - b.evaluate(t);
      }

      @Override
      public double differentiate(double t) {
        return a.differentiate(t) - b.differentiate(t);
      }
    };

  }

  /**
   * Creates a new function that computes the product of two functions.
   * 
   * @param a a differentiable function
   * @param b another differentiable function
   * 
   * @return a newly created function that computes the product <code>( a * b )</code>.
   */
  public static DifferentiableUnivariateFunction multiply(final DifferentiableUnivariateFunction a,
      final DifferentiableUnivariateFunction b) {
    return new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return a.evaluate(t) * b.evaluate(t);
      }

      @Override
      public double differentiate(double t) {
        return a.evaluate(t) * b.differentiate(t) + a.differentiate(t) * b.evaluate(t);
      }
    };
  }

  /**
   * Creates a new function that computes the quotient of two functions.
   * 
   * @param a a differentiable function, the dividend
   * @param b another differentiable function, the divisor
   * 
   * @return a newly created function that computes the quotient <code>( a / b )</code>
   * 
   */
  public static DifferentiableUnivariateFunction divide(final DifferentiableUnivariateFunction a,
      final DifferentiableUnivariateFunction b) {

    return new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return a.evaluate(t) / b.evaluate(t);
      }

      @Override
      public double differentiate(double t) {

        double bValue = b.evaluate(t);

        return (bValue * a.differentiate(t) - a.evaluate(t) * b.differentiate(t)) / bValue / bValue;
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
  public static DifferentiableUnivariateFunction compose(final DifferentiableUnivariateFunction a,
      final DifferentiableUnivariateFunction b) {
    return new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return a.evaluate(b.evaluate(t));
      }

      @Override
      public double differentiate(double t) {
        return a.differentiate(b.evaluate(t)) * b.differentiate(t);
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
  public static DifferentiableUnivariateFunction scale(final double scale,
      final DifferentiableUnivariateFunction function) {
    return new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return scale * function.evaluate(t);
      }

      @Override
      public double differentiate(double t) {
        return scale * function.differentiate(t);
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
  public static DifferentiableUnivariateFunction negate(
      final DifferentiableUnivariateFunction function) {
    return scale(-1.0, function);
  }

  /**
   * Creates a function that estimates the derivative by utilizing a quadratic approximating
   * function over some small, delta interval surrounding the value of interest.
   * <p>
   * This is the same as simply averaging the forward and backward derivative estimates usually
   * utilized to numerically estimate derivatives.
   * </p>
   * 
   * @param function the function to differentiate numerically
   * @param deltaT the delta in the domain argument to step away from the point of interest in
   *        evaluating the derivative
   * 
   * @return a newly created differentiable function that differentiates a quadratic approximate to
   *         estimate the derivative
   */
  public static DifferentiableUnivariateFunction quadraticApproximation(
      final UnivariateFunction function, final double deltaT) {

    return new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return function.evaluate(t);
      }

      @Override
      public double differentiate(double t) {

        /*
         * Approximate the derivative numerically by assuming a quadratic approximation. Evaluate
         * the function at t-deltaT and t+deltaT, combine the results to produce the derivative
         * estimate.
         */
        return 0.5 / deltaT * (function.evaluate(t + deltaT) - function.evaluate(t - deltaT));
      }
    };

  }

  /**
   * Simple interruptible delegate implementation
   */
  private static class InterruptibleFunctionDelegate implements DifferentiableUnivariateFunction {

    private final RuntimeInterruptionChecker checker;
    private final DifferentiableUnivariateFunction delegate;

    InterruptibleFunctionDelegate(RuntimeInterruptionChecker checker,
        DifferentiableUnivariateFunction delegate) {
      super();
      this.checker = checker;
      this.delegate = delegate;
    }

    @Override
    public double evaluate(double t) {
      checker.processRuntimeInterruptCheckAndThrow();
      return delegate.evaluate(t);
    }

    @Override
    public double differentiate(double t) {
      checker.processRuntimeInterruptCheckAndThrow();
      return delegate.differentiate(t);
    }

  }

  /**
   * Wraps an existing function and provides a variant that will respond to
   * {@link Thread#interrupt()} by throwing {@link RuntimeInterruptedException} on interrupt.
   * 
   * @param function the function to wrap into an interruptible one
   * @param evaluationCheckModulo the total number of derivative or function evaluations to execute
   *        before checking {@link Thread#interrupted()}
   * 
   * @return the interruptible function
   */
  public static DifferentiableUnivariateFunction interruptibleFunction(
      DifferentiableUnivariateFunction function, int evaluationCheckModulo) {
    return new InterruptibleFunctionDelegate(
        RuntimeInterruptionChecker.createWithCheckModulo(evaluationCheckModulo), function);
  }

  /**
   * Wraps an existing function and provides a variant that will respond to
   * {@link Thread#interrupt()} by throwing {@link RuntimeInterruptedException} on interrupt.
   * <p>
   * This variant is similar to
   * {@link DifferentiableUnivariateFunctions#interruptibleFunction(DifferentiableUnivariateFunction, int)}
   * except it checks {@link Thread#interrupted()} every call into
   * {@link DifferentiableUnivariateFunction#evaluate(double)} or
   * {@link DifferentiableUnivariateFunction#differentiate(double)}
   * </p>
   *
   * @param function the function to wrap
   * 
   * @return an interruptible function
   */
  public static DifferentiableUnivariateFunction interruptibleFunction(
      DifferentiableUnivariateFunction function) {
    return new InterruptibleFunctionDelegate(RuntimeInterruptionChecker.create(), function);
  }

  /**
   * Static field that retains the {@link PicanteMath#toDegrees(double)} wrapper function.
   */
  private static final DifferentiableUnivariateFunction MATH_TO_DEGREES =
      new DifferentiableUnivariateFunction() {

        @Override
        public double evaluate(double t) {
          return PicanteMath.toDegrees(t);
        }

        @Override
        public double differentiate(@SuppressWarnings("unused") double t) {
          /*
           * It's a simple scaling of the input argument, so just return the scale factor.
           */
          return PicanteMath.toDegrees(1.0);
        }
      };

  /**
   * Returns a {@link DifferentiableUnivariateFunction} that simply invokes
   * {@link PicanteMath#toDegrees(double)} and provides a proper derivative.
   * 
   * @return function that converts radians to degrees
   */
  public static DifferentiableUnivariateFunction toDegrees() {
    return MATH_TO_DEGREES;
  }

  /**
   * Static field that retains the {@link PicanteMath#toRadians(double)} wrapper function.
   */
  private static final DifferentiableUnivariateFunction MATH_TO_RADIANS =
      new DifferentiableUnivariateFunction() {

        @Override
        public double evaluate(double t) {
          return PicanteMath.toRadians(t);
        }

        @Override
        public double differentiate(@SuppressWarnings("unused") double t) {
          /*
           * It's a simple scaling of the input argument, so just return the scale factor.
           */
          return PicanteMath.toRadians(1.0);
        }
      };

  /**
   * Returns a {@link DifferentiableUnivariateFunction} that simply invokes
   * {@link PicanteMath#toRadians(double)} and provides a proper derivative.
   * 
   * @return function that converts degrees to radians
   */
  public static DifferentiableUnivariateFunction toRadians() {
    return MATH_TO_RADIANS;
  }

  /**
   * Static field that retains the {@link PicanteMath#cos(double)} wrapper function.
   */
  private static final DifferentiableUnivariateFunction COSINE =
      new DifferentiableUnivariateFunction() {

        @Override
        public double evaluate(double t) {
          return cos(t);
        }

        @Override
        public double differentiate(double t) {
          return -sin(t);
        }
      };

  /**
   * Returns a {@link DifferentiableUnivariateFunction} that simply invokes
   * {@link PicanteMath#cos(double)} and provides a proper derivative.
   * 
   * @return function that evaluates the cosine function
   */
  public static DifferentiableUnivariateFunction cosine() {
    return COSINE;
  }

  /**
   * Static field that retains the {@link PicanteMath#sin(double)} wrapper function.
   */
  private static final DifferentiableUnivariateFunction SINE =
      new DifferentiableUnivariateFunction() {

        @Override
        public double evaluate(double t) {
          return sin(t);
        }

        @Override
        public double differentiate(double t) {
          return cos(t);
        }
      };

  /**
   * Returns a {@link DifferentiableUnivariateFunction} that simply invokes
   * {@link PicanteMath#sin(double)} and provides a proper derivative.
   * 
   * @return function that evaluates the sine function
   */
  public static DifferentiableUnivariateFunction sine() {
    return SINE;
  }

}
