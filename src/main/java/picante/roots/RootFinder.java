package picante.roots;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

import org.apache.commons.math3.analysis.solvers.UnivariateSolver;

import com.google.common.base.Supplier;
import picante.math.functions.DifferentiableUnivariateFunction;
import picante.math.functions.DifferentiableUnivariateFunctions;
import picante.math.functions.UnivariateFunction;
import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;

/*
 * TODO: There is a fair amount of duplication of code between the methods on this implementation
 * that should be abstracted and cleaned up. Also I intend to add an absolute extrema search, and an
 * absolute extrema search with windows off the extrema are satisfied. This code also desperately
 * needs a collection of unit tests.
 */

/**
 * An object providing methods used to find the roots and ranges over the domain where constraints
 * on the value of the function are met. These include, finding local extrema, specific values, and
 * intervals where functions are less than or greater than values of interest.
 * <p>
 * When finding extrema, only extrema within the interior of the specified domain are located. If
 * you want to find values the include the endpoints, consider widening the interval with:
 * {@link IntervalSet#expand(double, double)} or {@link IntervalSet#expand(double)}.
 * </p>
 * <p>
 * With regards to step size, this is used to bracket zeros in the derivative ultimately to locate
 * intervals where the function is decreasing. So when searching for a root, the step size is not
 * used in the direct evaluation of the function.
 * </p>
 * <p>
 * If you need to apply the algorithms here to functions that do not implement
 * {@link UnivariateFunction}, consider using
 * {@link DifferentiableUnivariateFunctions#quadraticApproximation(UnivariateFunction, double)}. If
 * the function is truly not differentiable (discontinuities, kinks, etc.), then the methods in this
 * collection may not be suitable for the evaluation.
 * </p>
 * <p>
 * This class is intended to be a thread-safe provider.
 * </p>
 * 
 * @see Steppers
 */
public final class RootFinder {

  public static void main(String[] args) {

    RootFinder finder = create();

    DifferentiableUnivariateFunction sine = new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return -Math.sin(t);
      }

      @Override
      public double differentiate(double t) {
        double result = -Math.cos(t);
        if (Math.abs(result) < 1e-14) { // Constrain the derivative to be zero exactly if close
                                        // enough to test exceptional cases
          return 0.0;
        }

        return result;
      }
    };

    Stepper stepper = Steppers.createConstant(0.1);
    System.out.println(finder.locateLocalMinima(sine,
        IntervalSet.create(Math.PI / 2.0 - 0.001, 7 * Math.PI), stepper));
    System.out.println(finder.locateLocalMaxima(sine,
        IntervalSet.create(Math.PI / 2.0 - 0.001, 7 * Math.PI), stepper));

    System.out.println(finder.locateLocalExtrema(sine,
        IntervalSet.create(Math.PI / 2.0 - 0.001, 7 * Math.PI), stepper));
    System.out
        .println(finder.locateValue(sine, 0.25, IntervalSet.create(0.0, 6 * Math.PI), stepper));
    System.out.println(
        finder.locateLessThanValue(sine, 0.25, IntervalSet.create(0.0, 6 * Math.PI), stepper));
    System.out.println(finder.locateGreaterThanValue(
        DifferentiableUnivariateFunctions.quadraticApproximation(sine, 0.001), 0.25,
        IntervalSet.create(0.0, 6 * Math.PI), stepper));

    System.out.println(finder.locateMaxima(sine, IntervalSet.create(0.0, Math.PI), stepper));
    System.out.println(finder.locateMinima(sine, IntervalSet.create(0.0, Math.PI), stepper));
    System.out.println(
        finder.locateMaxima(sine, IntervalSet.create(-Math.PI / 2.0, Math.PI / 2.0), stepper));
    System.out.println(
        finder.locateMinima(sine, IntervalSet.create(-Math.PI / 2.0, Math.PI / 2.0), stepper));

  }

  /**
   * Maximum number of iterations to utilize in the solver.
   */
  private final int maximumSolverIterations = Integer.MAX_VALUE;

  /**
   * A supplier of solvers to utilize in each search. This abstraction is utilized to enable the
   * root finder to be thread-safe.
   */
  private final Supplier<? extends UnivariateSolver> solverSupplier;


  /**
   * Package private constructor that creates the finder.
   * <p>
   * This constructor is package private, as it forces users to utilize the static creation methods
   * provided.
   * </p>
   * 
   * @param solverSupplier the supplier of solvers. Note: this supplier is expected to produce
   *        solvers that are independent and &quot;thread-safe&quot; from one another.
   * 
   * @see RootFinder#create()
   */
  RootFinder(Supplier<? extends UnivariateSolver> solverSupplier) {
    this.solverSupplier = checkNotNull(solverSupplier, "Solver supplier may not be null");
  }

  /**
   * Creates a root finder with the default of a absolute accuracy of 1.0e-6.
   * 
   * @return the new root finder
   */
  public static RootFinder create() {
    return new RootFinder(new BrentSolverSupplier(0.0, 1.0e-6));
  }

  /**
   * Creates a root finder with the default
   * 
   * @param accuracy
   * 
   * @return the new root finder
   */
  public static RootFinder createWithAbsoluteAccuracy(double accuracy) {
    checkArgument(accuracy > 0.0, "Absolute accuracy must be strictly positive, was: %s", accuracy);
    return new RootFinder(new BrentSolverSupplier(0.0, accuracy));
  }

  /**
   * Locate local maxima interior to the specified domain for the function.
   * 
   * @param function the differentiable function to locate local maxima
   * @param domain the domain over which to perform the search
   * @param stepper the {@link Stepper} to use in the search.
   * 
   * @return an {@link IntervalSet} containing the local maxima interior to the domain. The windows
   *         here are guaranteed to be singleton
   */
  public IntervalSet locateLocalMaxima(DifferentiableUnivariateFunction function,
      IntervalSet domain, Stepper stepper) {

    if (domain.isEmpty()) {
      return IntervalSet.EMPTY;
    }
    IntervalSet.Builder result = IntervalSet.builder();
    IntervalSet.Builder workspace = IntervalSet.builder();
    UnivariateSolver solver = solverSupplier.get();

    /*
     * Determine the intervals where the function is decreasing. Only consider intervals with length
     * greater than zero.
     */
    for (UnwritableInterval interval : domain) {
      if (interval.getLength() > 0.0) {
        locateDecreasingIntervals(interval, function, stepper, solver, workspace, result);
      }
    }

    IntervalSet decreasingIntervals = result.build();

    result.empty();
    for (UnwritableInterval interval : decreasingIntervals) {
      if (!domain.onBoundary(interval.getBegin())) {
        result.add(interval.getBegin(), interval.getBegin());
      }
    }

    return result.build();

  }

  /**
   * Locate maxima over the specified domain for the function. This method is similar to
   * {@link RootFinder#locateLocalMaxima(DifferentiableUnivariateFunction, IntervalSet, Stepper)},
   * but in addition to locating local maxima in the interior of the domain, it adds end points of
   * intervals in the domain where the function value is non-increasing along the direction from the
   * boundary into the interior of the domain.
   * 
   * @param function the differentiable function to locate maxima
   * @param domain the domain over which to perform the search
   * @param stepper the {@link Stepper} to use in the search
   * 
   * @return an {@link IntervalSet} containing the maxima on the domain. The windows are guarenteed
   *         to be singleton.
   */
  public IntervalSet locateMaxima(DifferentiableUnivariateFunction function, IntervalSet domain,
      Stepper stepper) {

    if (domain.isEmpty()) {
      return IntervalSet.EMPTY;
    }

    /*
     * Start by placing all the local maxima interior to the domain into the result set.
     */
    IntervalSet.Builder resultBuilder =
        IntervalSet.builder(locateLocalMaxima(function, domain, stepper));

    /*
     * Loop over all the interval domain start points searching for points where the derivative is
     * zero or negative. Correspondingly check the end points for points where the derivative is
     * zero or positive.
     */
    for (UnwritableInterval interval : domain) {
      double begin = interval.getBegin();
      double end = interval.getEnd();
      double beginDerivative = function.differentiate(interval.getBegin());
      double endDerivative = function.differentiate(interval.getEnd());
      /*
       * Looking for a derivative at the beginning of the interval that is negative. If it's 0
       * exactly, then use the stepper and see if the value of the function actually decreases from
       * the interval start to one step into the interior.
       */
      if ((beginDerivative < 0) || (beginDerivative == 0 && function.evaluate(begin) > function
          .evaluate(Math.min(begin + stepper.step(begin), end)))) {
        resultBuilder.add(begin, begin);
      }

      /*
       * Looking for a derivative at the end of the interval that is positive. If it's 0 exactly,
       * then use the stepper and see if the value of the function actually increases from the
       * interval end to one step into the interior.
       */
      if ((endDerivative > 0) || (endDerivative == 0 && function
          .evaluate(Math.max(begin, end - stepper.step(end))) < function.evaluate(end))) {
        resultBuilder.add(interval.getEnd(), interval.getEnd());
      }
    }

    return resultBuilder.build();

  }

  /**
   * Locate global maxima on the specified domain for the function.
   * 
   * @param function the differentiable function to locate global maxima
   * @param domain the domain over which to perform the search
   * @param stepper the {@link Stepper} to use in the search
   * 
   * @return an {@link IntervalSet} containing the global maxima over the domain. The windows here
   *         are guaranteed to be singleton.
   */
  public IntervalSet locateGlobalMaxima(DifferentiableUnivariateFunction function,
      IntervalSet domain, Stepper stepper) {

    if (domain.isEmpty()) {
      return IntervalSet.EMPTY;
    }

    IntervalSet localMaxima = locateLocalMaxima(function, domain, stepper);

    IntervalSet.Builder resultBuilder = IntervalSet.builder();
    double maxValue = -Double.MAX_VALUE;

    /*
     * Locate the largest local maxima interior to the domain.
     */
    for (UnwritableInterval interval : localMaxima) {
      maxValue = updateMaximumValue(interval.getBegin(), function.evaluate(interval.getBegin()),
          resultBuilder, maxValue);
    }

    /*
     * And now consider all the end points of the domain.
     */
    for (UnwritableInterval interval : domain) {
      maxValue = updateMaximumValue(interval.getBegin(), function.evaluate(interval.getBegin()),
          resultBuilder, maxValue);
      maxValue = updateMaximumValue(interval.getEnd(), function.evaluate(interval.getEnd()),
          resultBuilder, maxValue);
    }

    return resultBuilder.build();

  }

  /**
   * Updates the contents of resultBuilder to include a new potential global maximum.
   * 
   * @param t the domain value of the current potential extrema under consideration
   * @param value the corresponding function value at t
   * @param resultBuilder the accumulating resultBuilder
   * @param maxValue the current maximum value
   * 
   * @return maxValue, potentially updated if value exceeds it.
   */
  private double updateMaximumValue(double t, double value, IntervalSet.Builder resultBuilder,
      double maxValue) {
    if (value > maxValue) {
      /*
       * The current value exceeds maxValue, dump any potential global extrema from the interval set
       * builder and update the value.
       */
      resultBuilder.empty();
      maxValue = value;
    }

    if (value == maxValue) {
      /*
       * Add the corresponding domain value to the global maxima interval set.
       */
      resultBuilder.add(t, t);
    }

    return maxValue;
  }



  /**
   * Locate local minima interior to the specified domain for the function.
   * 
   * @param function the differentiable function to locate local minima
   * @param domain the domain over which to perform the search
   * @param stepper the {@link Stepper} to use in the search.
   * 
   * @return an {@link IntervalSet} containing the local minima interior to the domain. The windows
   *         here are guaranteed to be singleton
   */
  public IntervalSet locateLocalMinima(DifferentiableUnivariateFunction function,
      IntervalSet domain, Stepper stepper) {

    if (domain.isEmpty()) {
      return IntervalSet.EMPTY;
    }
    IntervalSet.Builder result = IntervalSet.builder();
    IntervalSet.Builder workspace = IntervalSet.builder();
    UnivariateSolver solver = solverSupplier.get();

    /*
     * Determine the intervals where the function is decreasing.
     */
    for (UnwritableInterval interval : domain) {
      locateDecreasingIntervals(interval, function, stepper, solver, workspace, result);
    }

    IntervalSet decreasingIntervals = result.build();

    result.empty();
    for (UnwritableInterval interval : decreasingIntervals) {
      if (!domain.onBoundary(interval.getEnd())) {
        result.add(interval.getEnd(), interval.getEnd());
      }
    }

    return result.build();


  }

  /**
   * Locate maxima over the specified domain for the function. This method is similar to
   * {@link RootFinder#locateLocalMinima(DifferentiableUnivariateFunction, IntervalSet, Stepper)},
   * but in addition to locating local minima in the interior of the domain, it adds end points of
   * intervals in the domain where the function value is non-decreasing along the direction from the
   * boundary into the interior of the domain.
   * 
   * @param function the differentiable function to locate minima
   * @param domain the domain over which to perform the search
   * @param stepper the {@link Stepper} to use in the search
   * 
   * @return an {@link IntervalSet} containing the minima on the domain. The windows are guaranteed
   *         to be singleton.
   */
  public IntervalSet locateMinima(DifferentiableUnivariateFunction function, IntervalSet domain,
      Stepper stepper) {

    if (domain.isEmpty()) {
      return IntervalSet.EMPTY;
    }

    /*
     * Start by placing all the local minima interior to the domain into the result set.
     */
    IntervalSet.Builder resultBuilder =
        IntervalSet.builder(locateLocalMinima(function, domain, stepper));

    /*
     * Loop over all the interval domain start points searching for points where the derivative is
     * zero or positive. Correspondingly check the end points for points where the derivative is
     * zero or negative.
     */
    for (UnwritableInterval interval : domain) {
      double begin = interval.getBegin();
      double end = interval.getEnd();
      double beginDerivative = function.differentiate(interval.getBegin());
      double endDerivative = function.differentiate(interval.getEnd());
      /*
       * Looking for a derivative at the beginning of the interval that is positive. If it's 0
       * exactly, then use the stepper and see if the value of the function actually increases from
       * the interval start to one step into the interior.
       */
      if ((beginDerivative > 0) || (beginDerivative == 0 && function.evaluate(begin) < function
          .evaluate(Math.min(begin + stepper.step(begin), end)))) {
        resultBuilder.add(begin, begin);
      }

      /*
       * Looking for a derivative at the end of the interval that is negative. If it's 0 exactly,
       * then use the stepper and see if the value of the function actually decreases from the
       * interval end to one step into the interior.
       */
      if ((endDerivative < 0) || (endDerivative == 0 && function
          .evaluate(Math.max(begin, end - stepper.step(end))) > function.evaluate(end))) {
        resultBuilder.add(interval.getEnd(), interval.getEnd());
      }
    }

    return resultBuilder.build();

  }



  /**
   * Locate global minima on the specified domain for the function.
   * 
   * @param function the differentiable function to locate global minima
   * @param domain the domain over which to perform the search
   * @param stepper the {@link Stepper} to use in the search
   * 
   * @return an {@link IntervalSet} containing the global minima over the domain. The windows here
   *         are guaranteed to be singleton.
   */
  public IntervalSet locateGlobalMinima(DifferentiableUnivariateFunction function,
      IntervalSet domain, Stepper stepper) {

    if (domain.isEmpty()) {
      return IntervalSet.EMPTY;
    }

    IntervalSet localMinima = locateLocalMinima(function, domain, stepper);

    IntervalSet.Builder resultBuilder = IntervalSet.builder();
    double minValue = Double.MAX_VALUE;

    /*
     * Locate the largest local maxima interior to the domain.
     */
    for (UnwritableInterval interval : localMinima) {
      minValue = updateMinimumValue(interval.getBegin(), function.evaluate(interval.getBegin()),
          resultBuilder, minValue);
    }

    /*
     * And now consider all the end points of the domain.
     */
    for (UnwritableInterval interval : domain) {
      minValue = updateMinimumValue(interval.getBegin(), function.evaluate(interval.getBegin()),
          resultBuilder, minValue);
      minValue = updateMinimumValue(interval.getEnd(), function.evaluate(interval.getEnd()),
          resultBuilder, minValue);
    }

    return resultBuilder.build();

  }

  /**
   * Updates the contents of resultBuilder to include a new potential global minimum.
   * 
   * @param t the domain value of the current potential extrema under consideration
   * @param value the corresponding function value at t
   * @param resultBuilder the accumulating resultBuilder
   * @param minValue the current minimum value
   * 
   * @return minValue, potentially updated if value precedes it.
   */
  private double updateMinimumValue(double t, double value, IntervalSet.Builder resultBuilder,
      double minValue) {
    if (value < minValue) {
      /*
       * The current value exceeds maxValue, dump any potential global extrema from the interval set
       * builder and update the value.
       */
      resultBuilder.empty();
      minValue = value;
    }

    if (value == minValue) {
      /*
       * Add the corresponding domain value to the global maxima interval set.
       */
      resultBuilder.add(t, t);
    }

    return minValue;
  }



  /**
   * Locate local extrema interior to the specified domain for the function.
   * 
   * @param function the differentiable function to locate local extrema
   * @param domain the domain over which to perform the search
   * @param stepper the {@link Stepper} to use in the search.
   * 
   * @return an {@link IntervalSet} containing the local extrema interior to the domain. The windows
   *         here are guaranteed to be singleton
   */
  public IntervalSet locateLocalExtrema(DifferentiableUnivariateFunction function,
      IntervalSet domain, Stepper stepper) {

    if (domain.isEmpty()) {
      return IntervalSet.EMPTY;
    }
    IntervalSet.Builder result = IntervalSet.builder();
    IntervalSet.Builder workspace = IntervalSet.builder();
    UnivariateSolver solver = solverSupplier.get();

    /*
     * Determine the intervals where the function is decreasing.
     */
    for (UnwritableInterval interval : domain) {
      locateDecreasingIntervals(interval, function, stepper, solver, workspace, result);
    }

    IntervalSet decreasingIntervals = result.build();

    result.empty();
    for (UnwritableInterval interval : decreasingIntervals) {
      if (!domain.onBoundary(interval.getBegin())) {
        result.add(interval.getBegin(), interval.getBegin());
      }
      if (!domain.onBoundary(interval.getEnd())) {
        result.add(interval.getEnd(), interval.getEnd());
      }
    }

    return result.build();

  }

  /**
   * Locate entries in the domain where the value of the specified function is equal to value
   * 
   * @param function the function to locate the desired value
   * @param value the value of interest
   * @param domain the domain over which to perform the search
   * @param stepper the {@link Stepper} to use in the search.
   * 
   * @return an {@link IntervalSet} containing values from the domain where function evaluates to
   *         value. The entries are guaranteed to be singletons.
   */
  public IntervalSet locateValue(DifferentiableUnivariateFunction function, double value,
      IntervalSet domain, Stepper stepper) {

    if (domain.isEmpty()) {
      return IntervalSet.EMPTY;
    }
    IntervalSet.Builder result = IntervalSet.builder();
    IntervalSet.Builder workspace = IntervalSet.builder();
    UnivariateSolver solver = solverSupplier.get();

    /*
     * Determine the intervals where the function is decreasing.
     */
    for (UnwritableInterval interval : domain) {
      locateDecreasingIntervals(interval, function, stepper, solver, workspace, result);
    }

    /*
     * And now determine where it is increasing as well. This is just the complement against the
     * domain interval set supplied in the argument list.
     */
    IntervalSet decreasing = result.build();
    result.setTo(domain);
    result.difference(decreasing);
    IntervalSet increasing = result.build();

    /*
     * Accumulate the coverage intervals.
     */
    result.empty();
    org.apache.commons.math3.analysis.UnivariateFunction zero =
        subtractConstantAndAdapt(function, value);
    for (UnwritableInterval interval : decreasing) {

      /*
       * Check the end points of the interval. We know the function is monotonically decreasing over
       * the interval, so a search may not be necessary.
       */
      double hiValue = function.evaluate(interval.getBegin());
      double lowValue = function.evaluate(interval.getEnd());

      /*
       * Handle the exceptional cases first before engaging in the search.
       */
      if (lowValue == value) {
        result.add(interval.getEnd(), interval.getEnd());
      } else if (hiValue == value) {
        result.add(interval.getBegin(), interval.getBegin());
      } else if ((lowValue < value) && (hiValue > value)) {
        double root =
            solver.solve(maximumSolverIterations, zero, interval.getBegin(), interval.getEnd());
        result.add(root, root);
      }

    }

    for (UnwritableInterval interval : increasing) {

      /*
       * Check the end points of the interval. We know the function is monotonically increasing over
       * the interval, so a search may not be necessary.
       */
      double lowValue = function.evaluate(interval.getBegin());
      double hiValue = function.evaluate(interval.getEnd());

      /*
       * Handle the exceptional cases first before engaging in the search.
       */
      if (lowValue == value) {
        result.add(interval.getBegin(), interval.getBegin());
      } else if (hiValue == value) {
        result.add(interval.getEnd(), interval.getEnd());
      } else if ((lowValue < value) && (hiValue > value)) {
        double root =
            solver.solve(maximumSolverIterations, zero, interval.getBegin(), interval.getEnd());
        result.add(root, root);
      }

    }

    return result.build();

  }

  /**
   * Locate intervals where the specified function is less than the requested value.
   * 
   * @param function the function of interest
   * @param value the value to search for when the function is less than
   * @param domain the domain over which to perform the search
   * @param stepper the {@link Stepper} to use in the search.
   * 
   * @return an {@link IntervalSet} where function is less than value.
   */
  public IntervalSet locateGreaterThanValue(DifferentiableUnivariateFunction function, double value,
      IntervalSet domain, Stepper stepper) {

    if (domain.isEmpty()) {
      return IntervalSet.EMPTY;
    }
    IntervalSet.Builder result = IntervalSet.builder();
    IntervalSet.Builder workspace = IntervalSet.builder();
    UnivariateSolver solver = solverSupplier.get();

    /*
     * Determine the intervals where the function is decreasing.
     */
    for (UnwritableInterval interval : domain) {
      locateDecreasingIntervals(interval, function, stepper, solver, workspace, result);
    }

    /*
     * And now determine where it is increasing as well. This is just the complement against the
     * domain interval set supplied in the argument list.
     */
    IntervalSet decreasing = result.build();
    result.setTo(domain);
    result.difference(decreasing);
    IntervalSet increasing = result.build();

    /*
     * Accumulate the coverage intervals.
     */
    result.empty();
    org.apache.commons.math3.analysis.UnivariateFunction zero =
        subtractConstantAndAdapt(function, value);
    for (UnwritableInterval interval : decreasing) {

      /*
       * Check the end points of the interval. We know the function is monotonically decreasing over
       * the interval, so a search may not be necessary.
       */
      double hiValue = function.evaluate(interval.getBegin());
      double lowValue = function.evaluate(interval.getEnd());

      if (lowValue > value) {
        result.add(interval);
      } else if (hiValue > value) {
        /*
         * A search is required, since hiValue is above or equal to value.
         */
        result.add(interval.getBegin(),
            solver.solve(maximumSolverIterations, zero, interval.getBegin(), interval.getEnd()));
      }

    }

    for (UnwritableInterval interval : increasing) {

      /*
       * Check the end points of the interval. We know the function is monotonically increasing over
       * the interval, so a search may not be necessary.
       */
      double lowValue = function.evaluate(interval.getBegin());
      double hiValue = function.evaluate(interval.getEnd());

      if (lowValue > value) {
        result.add(interval);
      } else if (hiValue > value) {
        /*
         * A search is required, since hiValue is above or equal to value.
         */
        result.add(
            solver.solve(maximumSolverIterations, zero, interval.getBegin(), interval.getEnd()),
            interval.getEnd());
      }

    }

    return result.build();

  }

  // TODO: Implement this.
  // public IntervalSet locateWithinIBounds(DifferentiableUnivariateFunction function,
  // UnwritableInterval bounds, IntervalSet domain, Stepper stepper) {
  //
  // }

  /**
   * Locate intervals where the specified function is less than the requested value.
   * 
   * @param function the function of interest
   * @param value the value to search for when the function is less than
   * @param domain the domain over which to perform the search
   * @param stepper the {@link Stepper} to use in the search.
   * 
   * @return an {@link IntervalSet} where function is less than value.
   */
  public IntervalSet locateLessThanValue(DifferentiableUnivariateFunction function, double value,
      IntervalSet domain, Stepper stepper) {

    if (domain.isEmpty()) {
      return IntervalSet.EMPTY;
    }
    IntervalSet.Builder result = IntervalSet.builder();
    IntervalSet.Builder workspace = IntervalSet.builder();
    UnivariateSolver solver = solverSupplier.get();

    /*
     * Determine the intervals where the function is decreasing.
     */
    for (UnwritableInterval interval : domain) {
      locateDecreasingIntervals(interval, function, stepper, solver, workspace, result);
    }

    /*
     * And now determine where it is increasing as well. This is just the complement against the
     * domain interval set supplied in the argument list.
     */
    IntervalSet decreasing = result.build();
    result.setTo(domain);
    result.difference(decreasing);
    IntervalSet increasing = result.build();

    /*
     * Accumulate the coverage intervals.
     */
    result.empty();
    org.apache.commons.math3.analysis.UnivariateFunction zero =
        subtractConstantAndAdapt(function, value);
    for (UnwritableInterval interval : decreasing) {

      /*
       * Check the end points of the interval. We know the function is monotonically decreasing over
       * the interval, so a search may not be necessary.
       */
      double hiValue = function.evaluate(interval.getBegin());
      double lowValue = function.evaluate(interval.getEnd());

      if (hiValue < value) {
        result.add(interval);
      } else if (lowValue < value) {
        /*
         * A search is required, since hiValue is above or equal to value.
         */
        result.add(
            solver.solve(maximumSolverIterations, zero, interval.getBegin(), interval.getEnd()),
            interval.getEnd());
      }

    }

    for (UnwritableInterval interval : increasing) {

      /*
       * Check the end points of the interval. We know the function is monotonically increasing over
       * the interval, so a search may not be necessary.
       */
      double lowValue = function.evaluate(interval.getBegin());
      double hiValue = function.evaluate(interval.getEnd());

      if (hiValue < value) {
        result.add(interval);
      } else if (lowValue < value) {
        /*
         * A search is required, since hiValue is above or equal to value.
         */
        result.add(interval.getBegin(),
            solver.solve(maximumSolverIterations, zero, interval.getBegin(), interval.getEnd()));
      }

    }

    return result.build();

  }

  /**
   * Locate intervals where the specified function is decreasing in value.
   * 
   * @param function the function of interest
   * @param domain the domain over which to perform the search
   * @param stepper the {@link Stepper} to use in the search.
   * 
   * @return an {@link IntervalSet} where function is decreasing in value
   */
  public IntervalSet locateDecreasingIntervals(DifferentiableUnivariateFunction function,
      IntervalSet domain, Stepper stepper) {

    if (domain.isEmpty()) {
      return IntervalSet.EMPTY;
    }
    IntervalSet.Builder result = IntervalSet.builder();
    IntervalSet.Builder workspace = IntervalSet.builder();
    UnivariateSolver solver = solverSupplier.get();

    /*
     * Determine the intervals where the function is decreasing.
     */
    for (UnwritableInterval interval : domain) {
      locateDecreasingIntervals(interval, function, stepper, solver, workspace, result);
    }

    return result.build();

  }

  /**
   * Locate intervals where the specified function is increasing in value.
   * 
   * @param function the function of interest
   * @param domain the domain over which to perform the search
   * @param stepper the {@link Stepper} to use in the search
   * 
   * @return an {@link IntervalSet} where function is increasing in value
   */
  public IntervalSet locateIncreasingIntervals(DifferentiableUnivariateFunction function,
      IntervalSet domain, Stepper stepper) {
    return domain.difference(locateDecreasingIntervals(function, domain, stepper));
  }

  /**
   * Locates decreasing intervals of the function over the domain interval. Results are added to the
   * supplied result {@link IntervalSet.Builder}
   * 
   * @param domain the domain interval over which to perform the search
   * @param function the function to examine
   * @param stepper the stepper to use in the search
   * @param solver the solver to bracket the derivative roots
   * @param workspace an {@link IntervalSet.Builder} used as a workspace
   * @param result an {@link IntervalSet.Builder} used to accumulate the results
   */
  private void locateDecreasingIntervals(UnwritableInterval domain,
      DifferentiableUnivariateFunction function, Stepper stepper, UnivariateSolver solver,
      IntervalSet.Builder workspace, IntervalSet.Builder result) {

    /*
     * First locate the zeroes of the derivative on the domain.
     */
    workspace.empty();
    locateZeroes(domain, DifferentiableUnivariateFunctions.derivative(function), stepper, solver,
        workspace);

    /*
     * Determine within domain where the function is increasing. First handle the exceptional case
     * where workspace is empty.
     */
    if (workspace.size() == 0) {
      if (function.differentiate(domain.getMiddle()) < 0) {
        result.add(domain);
      }
      return;
    }

    /*
     * The points in workspace are the possible transitions from increasing to decreasing. Look at
     * the points between the entries and determine whether the function is decreasing there.
     */
    double begin = domain.getBegin();

    int size = workspace.size();
    for (int i = 0; i < size; i++) {
      UnwritableInterval interval = workspace.get(i);

      /*
       * Only compare if this is not the start of the interval.
       */
      if (interval.getBegin() != begin) {
        if (function.differentiate((interval.getBegin() + begin) / 2.0) < 0.0) {
          result.add(begin, interval.getBegin());
        }
      }

      begin = interval.getBegin();
    }

    if (begin != domain.getEnd()) {
      if (function.differentiate((begin + domain.getEnd()) / 2.0) < 0.0) {
        result.add(begin, domain.getEnd());
      }
    }

  }

  /**
   * Locates zeroes of the specified function over the supplied domain using the stepper and solver.
   * <p>
   * This method starts at the beginning of the domain interval, and steps through it looking to
   * bracket sign changes. The resultant builder that is supplied to the method only has values
   * appended to it. This enables a higher level method to loop over a collection of disjoint
   * intervals and accumulate the results in a single builder.
   * </p>
   * 
   * @param domain the interval over which to search
   * @param function the function of which zeroes are sought
   * @param stepper the stepper used to move time forward
   * @param solver the solver used to locate bracketed zeroes
   * @param result the {@link IntervalSet} builder to capture the zero points.
   * 
   */
  private void locateZeroes(UnwritableInterval domain, UnivariateFunction function, Stepper stepper,
      UnivariateSolver solver, IntervalSet.Builder result) {

    /*
     * Adapt the commons.math univariate function interface to the crucible one.
     */
    org.apache.commons.math3.analysis.UnivariateFunction adapter = adapt(function);

    /*
     * Initialize the search. Locate the value of the function at t.
     */
    double t = domain.getBegin();
    double end = domain.getEnd();

    double fT = function.evaluate(t);
    if (fT == 0) {
      result.add(t, t);
    }

    /*
     * Execute the loop through the interval stepping as prescribed by the stepper.
     */
    while (t < end) {

      double nextT = Math.min(stepper.step(t) + t, end);
      double fNextT = function.evaluate(nextT);

      /*
       * If the value of the function at nextT is 0, then add it to the list of zeroes.
       */
      if (fNextT == 0.0) {
        result.add(nextT, nextT);
      }

      /*
       * Only bother performing a refinement if either end of the current sub-interval is not zero,
       * and they have differing signs. We have to be careful, it's unlikely but possible that t and
       * nextT are one Math.ulp() apart. In this case, we just return whichever one is closer to
       * zero in magnitude.
       */
      if ((fT != 0.0) && (fNextT != 0.0) && (Math.signum(fT) != Math.signum(fNextT))) {

        double midPoint = (t + nextT) / 2.0;
        if ((midPoint == t) || (midPoint == nextT)) {
          /*
           * If we reach here than t and nextT are within one Math.ulp() of each other and
           * bracketing a zero. Add the one that is smaller in absolute value to the result.
           */
          if (Math.abs(fT) < Math.abs(fNextT)) {
            result.add(t, t);
          } else {
            result.add(nextT, nextT);
          }
        } else {
          /*
           * We have bracketed a zero, locate it.
           */
          double zero = solver.solve(maximumSolverIterations, adapter, t, nextT);
          result.add(zero, zero);
        }
      }

      /*
       * Update the value of f(t) and t prior to entering the top of the loop.
       */
      fT = fNextT;
      t = nextT;
    }

  }

  /**
   * Adapts the supplied function, after subtracting the specified constant.
   * 
   * @param function the {@link UnivariateFunction} to adapt
   * @param value the constant to subtract
   * 
   * @return an adapted function that plugs into the commons-math {@link UnivariateSolver} framework
   */
  private static org.apache.commons.math3.analysis.UnivariateFunction subtractConstantAndAdapt(
      final UnivariateFunction function, final double value) {
    return new org.apache.commons.math3.analysis.UnivariateFunction() {

      @Override
      public double value(double x) {
        return function.evaluate(x) - value;
      }
    };
  }

  /**
   * Adapts {@link UnivariateFunction} to the equivalent commons.math interface:
   * {@link org.apache.commons.math3.analysis.UnivariateFunction}.
   * 
   * @param function crucible interface to adapt
   * 
   * @return commons.math interface
   */
  private static org.apache.commons.math3.analysis.UnivariateFunction adapt(
      final UnivariateFunction function) {
    return new org.apache.commons.math3.analysis.UnivariateFunction() {

      @Override
      public double value(double x) {
        return function.evaluate(x);
      }
    };
  }


}
