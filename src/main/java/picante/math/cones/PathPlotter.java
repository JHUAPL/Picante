package picante.math.cones;

import java.awt.geom.Point2D;
import picante.math.intervals.UnwritableInterval;

/**
 * Interface describing a parameterized 2D path.
 */
interface PathPlotter {

  /**
   * Computes a point along a parameterized path.
   * 
   * @param p the value of the parameter at which to compute the point
   * @param buffer a buffer to receive the results
   * 
   * @return a reference to the supplied buffer for convenience of method chaining
   * 
   * @throws IllegalArgumentException if p is not in the domain supported by this function.
   */
  Point2D plot(double p, Point2D buffer);

  default Point2D plot(double p) {
    return plot(p, new Point2D.Double());
  }

  /**
   * Retrieves the domain over which the path's {@link PathPlotter#plot(double, Point2D)} function
   * is to be invoked.
   * 
   * @return the domain over which the plotter is to be plotted
   */
  UnwritableInterval getDomain();

}
