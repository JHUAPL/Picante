package picante.roots;

import picante.math.functions.UnivariateFunction;
import picante.math.intervals.IntervalSet;

/**
 * Collection of static utility methods for operating on and creating
 * {@link UnivariateBooleanFunction}s.
 */
public class UnivariateBooleanFunctions {

  private static final UnivariateBooleanFunction FALSE_FUNCTION = new UnivariateBooleanFunction() {

    @Override
    public boolean evaluate(@SuppressWarnings("unused") double t) {
      return false;
    }
  };

  /**
   * Returns a function that always evaluates to false.
   * 
   * @return a function that evaluates to false.
   */
  public static UnivariateBooleanFunction alwaysFalse() {
    return FALSE_FUNCTION;
  }

  private static final UnivariateBooleanFunction TRUE_FUNCTION = new UnivariateBooleanFunction() {

    @Override
    public boolean evaluate(@SuppressWarnings("unused") double t) {
      return true;
    }
  };

  /**
   * Returns a function that always evaluates to true.
   * 
   * @return a function that evaluates to true.
   */
  public static UnivariateBooleanFunction alwaysTrue() {
    return TRUE_FUNCTION;
  }

  /**
   * Create a new function that negates the supplied function.
   * 
   * @param function function to negate
   * 
   * @return a newly created function that returns !function.evaluate(t)
   */
  public static UnivariateBooleanFunction negate(final UnivariateBooleanFunction function) {
    return new UnivariateBooleanFunction() {

      @Override
      public boolean evaluate(double t) {
        return !function.evaluate(t);
      }
    };
  }

  /**
   * Create a function that evaluates to true when the input argument is contained within the
   * supplied interval set and to false otherwise.
   * 
   * @param set the interval set in question
   * 
   * @return
   */
  public static UnivariateBooleanFunction fromIntervalSet(final IntervalSet set) {
    return new UnivariateBooleanFunction() {

      @Override
      public boolean evaluate(double t) {
        return set.contains(t);
      }
    };
  }

  /**
   * Creates a function that evaluates to true whenever the supplied {@link UnivariateFunction}
   * evaluates to a quantity less than a threshold.
   * 
   * @param function the function
   * @param threshold the threshold
   * 
   * @return a function that evaluates to true whenever {@link UnivariateFunction#evaluate(double)}
   *         is less than threshold.
   */
  public static UnivariateBooleanFunction lessThanThreshold(final UnivariateFunction function,
      final double threshold) {
    return new UnivariateBooleanFunction() {

      @Override
      public boolean evaluate(double t) {
        return function.evaluate(t) < threshold;
      }
    };
  }

  /**
   * Creates a function that evaluates to true whenever the supplied {@link UnivariateFunction}
   * evaluates to a quantity greater than threshold.
   * 
   * @param function the function
   * @param threshold the threshold
   * 
   * @return a function that evaluates to true whenever {@link UnivariateFunction#evaluate(double)}
   *         is greater than threshold.
   */
  public static UnivariateBooleanFunction greaterThanThreshold(final UnivariateFunction function,
      final double threshold) {
    return new UnivariateBooleanFunction() {

      @Override
      public boolean evaluate(double t) {
        return function.evaluate(t) > threshold;
      }
    };
  }

  /**
   * Creates a function that evaluates to true whenever the supplied {@link UnivariateFunction}
   * evaluates to a quantity less than or equal to a threshold.
   * 
   * @param function the function
   * @param threshold the threshold
   * 
   * @return a function that evaluates to true whenever {@link UnivariateFunction#evaluate(double)}
   *         is less than threshold.
   */
  public static UnivariateBooleanFunction lessThanOrEqualToThreshold(
      final UnivariateFunction function, final double threshold) {
    return new UnivariateBooleanFunction() {

      @Override
      public boolean evaluate(double t) {
        return function.evaluate(t) <= threshold;
      }
    };
  }

  /**
   * Creates a function that evaluates to true whenever the supplied {@link UnivariateFunction}
   * evaluates to a quantity greater than or equal to a threshold.
   * 
   * @param function the function
   * @param threshold the threshold
   * 
   * @return a function that evaluates to true whenever {@link UnivariateFunction#evaluate(double)}
   *         is greater than threshold.
   */
  public static UnivariateBooleanFunction greaterThanOrEqualToThreshold(
      final UnivariateFunction function, final double threshold) {
    return new UnivariateBooleanFunction() {

      @Override
      public boolean evaluate(double t) {
        return function.evaluate(t) >= threshold;
      }
    };
  }

  /**
   * Creates a function that evaluates to true whenever the supplied {@link UnivariateFunction}
   * evaluates to a quantity equal to a value.
   * 
   * @param function the function
   * @param value the value
   * 
   * @return a function that evaluates to true whenever {@link UnivariateFunction#evaluate(double)}
   *         is equal to value.
   */
  public static UnivariateBooleanFunction equalToValue(final UnivariateFunction function,
      final double value) {
    return new UnivariateBooleanFunction() {

      @Override
      public boolean evaluate(double t) {
        return function.evaluate(t) == value;
      }
    };
  }

  /**
   * Creates a function that evaluates to true whenever the supplied {@link UnivariateFunction}
   * evaluates to a quantity that is not equal to a value.
   * 
   * @param function the function
   * @param value the value
   * 
   * @return a function that evaluates to true whenever {@link UnivariateFunction#evaluate(double)}
   *         is not equal to value.
   */
  public static UnivariateBooleanFunction notEqualToValue(final UnivariateFunction function,
      final double value) {
    return new UnivariateBooleanFunction() {

      @Override
      public boolean evaluate(double t) {
        return function.evaluate(t) != value;
      }
    };
  }

}
