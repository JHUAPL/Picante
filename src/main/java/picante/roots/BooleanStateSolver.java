package picante.roots;

import com.google.common.base.Preconditions;
import picante.math.intervals.Interval;

/**
 * Class that encapsulates the search logic to locate state transitions in a
 * {@link UnivariateBooleanFunction}.
 */
class BooleanStateSolver {

  private final double tolerance;

  BooleanStateSolver(double tolerance) {
    Preconditions.checkArgument(tolerance > 0, "Tolerance must be strictly positive.");
    this.tolerance = tolerance;
  }

  /**
   * Searches for a state transition point in function.
   * 
   * @param left the left bracketing value
   * @param right the right bracketing value
   * @param function the function to search for a state transition
   * 
   * @return an interval that brackets the actual transition argument that is of length less than
   *         tolerance and is such that function.evaluate(interval.getBegin()) !=
   *         function.evalute(interval.getEnd())
   * 
   * @throws IllegalArgumentException if left &ge; right or function(left) == function(right)
   */
  Interval search(double left, double right, UnivariateBooleanFunction function, Interval buffer) {

    Preconditions.checkArgument(left < right, "Left must preceed right.");

    /*
     * Utilize binary search over the interval [left, right] to locate a state change in function.
     * Continue to contract the intervals until the points are within tolerance apart.
     */
    boolean leftState = function.evaluate(left);
    boolean rightState = function.evaluate(right);

    /*
     * Check to see that leftState and rightState are different. If they are not, then throw an
     * illegal argument exception as the caller has made a mistake.
     */
    Preconditions.checkArgument(leftState != rightState,
        "Inconsistent states supplied, supplied times must bracket a state change in function");

    double middle = (right + left) / 2.0;

    while ((right - left) > tolerance) {

      boolean middleState = function.evaluate(middle);

      /*
       * Adjust the value of left or right depending on which value the function returned.
       */
      if (middleState == leftState) {
        left = middle;
      } else {
        right = middle;
      }

      middle = (left + right) / 2.0;

    }

    buffer.set(left, right);
    return buffer;

  }

  double getTolerance() {
    return tolerance;
  }

}
