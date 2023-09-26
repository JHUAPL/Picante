package picante.roots;

import picante.math.intervals.Interval;
import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;

/**
 * An object providing methods used to find transitions in {@link UnivariateBooleanFunction}s and
 * reporting the intervals over which these functions are either true or false.
 * <p>
 * With regards to step size, this is used to bracket all state transitions. Thus, you should supply
 * a stepper implementation that is guaranteed to produces steps that are less than the largest
 * interval of interest.
 * </p>
 * 
 * @see Steppers
 */
public class BooleanStateFinder {

  private final BooleanStateSolver solver;

  private BooleanStateFinder(BooleanStateSolver solver) {
    this.solver = solver;
  }

  /**
   * Creates a state finder with the default absolute accuracy of 1.0e-6.
   * 
   * @return the new state finder
   */
  public static BooleanStateFinder create() {
    return new BooleanStateFinder(new BooleanStateSolver(1e-6));
  }

  /**
   * Creates a state finder with the specified accuracy.
   * 
   * @param accuracy the accuracy to utilize in binary searches
   * 
   * @return the new state finder with requested accuracy
   * 
   * @throws IllegalArgumentException if accuracy is not strictly positive.
   */
  public static BooleanStateFinder createWithAbsoluteAccuracy(double accuracy) {
    return new BooleanStateFinder(new BooleanStateSolver(accuracy));
  }

  /**
   * Locates when the supplied function evaluates to true over the supplied domain.
   * 
   * @param function the function to determine when it evaluates to true
   * @param domain the domain over which the search is to be performed
   * @param stepper the implementation of {@link Stepper} that is to be utilized.
   * 
   * @return the {@link IntervalSet} containing intervals from the subset of domain when
   *         function.evaluate() is true.
   */
  public IntervalSet locateTransitionToTrue(UnivariateBooleanFunction function, IntervalSet domain,
      Stepper stepper) {

    /*
     * Handle the trivial case, if the user requested an empty domain over which to search, then
     * there is nothing to find.
     */
    if (domain.isEmpty()) {
      return IntervalSet.EMPTY;
    }

    /*
     * Setup classes used to collect results.
     */
    IntervalSet.Builder resultBuilder = IntervalSet.builder();
    Interval transitionBracket = new Interval();

    for (UnwritableInterval interval : domain) {
      processIntervalSearchForTruth(function, stepper, interval, resultBuilder, transitionBracket);
    }

    return resultBuilder.build();

  }

  /**
   * Method that encapsulates the processing of a single interval of coverage from the domain.
   * 
   * @param function the function to analyze
   * @param stepper the stepper to drive stepping
   * @param interval the interval of which analysis is to be performed
   * @param resultBuilder the result builder to capture any true intervals
   * @param transitionBracket a buffer to hold some intermediate results, passed in to avoid memory
   *        allocation
   */
  private void processIntervalSearchForTruth(UnivariateBooleanFunction function, Stepper stepper,
      UnwritableInterval interval, IntervalSet.Builder resultBuilder, Interval transitionBracket) {

    /*
     * If the requested interval is a singleton, then simply add it to the result builder if it
     * evaluates to true.
     */

    if (interval.getLength() == 0) {
      if (function.evaluate(interval.getBegin())) {
        resultBuilder.add(interval);
      }
      return;
    }

    /*
     * At this point we have an actual interval to process, start stepping.
     */

    double start = interval.getBegin();
    double end = interval.getEnd();

    while (start < end) {

      /*
       * Evaluate the function at start to determine the initial state.
       */
      boolean startState = function.evaluate(start);

      /*
       * Step forward until argument reaches end or a state change is uncovered.
       */
      boolean state = startState;
      double previousArgument = start;
      double argument = start;
      boolean done = false;

      while ((state == startState) && (!done)) {

        previousArgument = argument;
        argument += stepper.step(argument);

        /*
         * Check the exit condition.
         */
        if (argument >= end) {
          argument = end;
          done = true;
        }

        state = function.evaluate(argument);
      }

      /*
       * At this point a state change has been bracketed, or argument exceeded end.
       */
      if (!done) {

        /*
         * previousArgument and argument bracket a state change in function. Locate the transition
         * time.
         */
        solver.search(previousArgument, argument, function, transitionBracket);

        argument = transitionBracket.getEnd();

        /*
         * Determine whether the interval cut is to be added to the output schedule.
         */
        if (startState) {
          resultBuilder.add(start, transitionBracket.getMiddle());
        }

      } else {

        /*
         * This is complicated, largely because we're trying to capture any lingering small (less
         * than stepper requested step size) state transition at the end of the interval in the
         * domain.
         */
        if (state != startState) {
          /*
           * The two states do not agree, thus bracketing a transition in the last potentially
           * partial step. Perform the necessary refinement and add any small interval to the
           * result.
           */
          solver.search(previousArgument, argument, function, transitionBracket);

          /*
           * Add the appropriate interval on either side of the transition.
           */
          if (startState) {
            resultBuilder.add(start, transitionBracket.getMiddle());
          } else {
            resultBuilder.add(transitionBracket.getMiddle(), end);
          }

        } else {
          /*
           * Since the two states are equal, add the interval only if startState is true.
           */
          if (startState) {
            resultBuilder.add(start, end);
          }
        }

      }

      /*
       * Update start to argument. This will either be end, or the right edge of the transition
       * bracketing interval.
       */
      start = argument;

    }

  }


  /**
   * Locates when the supplied function evaluates to false over the supplied domain.
   * 
   * @param function the function to determine when it evaluates to false
   * @param domain the domain over which the search is to be performed
   * @param stepper the implementation of {@link Stepper} that is to be utilized.
   * 
   * @return the {@link IntervalSet} containing intervals from the subset of domain when
   *         function.evaluate() is false.
   */
  public IntervalSet locateTransitionToFalse(final UnivariateBooleanFunction function,
      IntervalSet domain, Stepper stepper) {
    return locateTransitionToTrue(UnivariateBooleanFunctions.negate(function), domain, stepper);
  }

  public IntervalSet locateTrue(UnivariateBooleanFunction function, IntervalSet domain,
      Stepper stepper) {
    return locateTransitionToTrue(function, domain, stepper).contract(solver.getTolerance() * 2.0);
  }

  public IntervalSet locateFalse(UnivariateBooleanFunction function, IntervalSet domain,
      Stepper stepper) {
    return locateTransitionToFalse(function, domain, stepper).contract(solver.getTolerance() * 2.0);
  }
}
