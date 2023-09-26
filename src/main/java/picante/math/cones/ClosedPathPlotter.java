package picante.math.cones;

import java.awt.geom.Point2D;
import picante.math.intervals.UnwritableInterval;

/**
 * Interface describing a closed parameterized path in 2D.
 */
interface ClosedPathPlotter extends PathPlotter {

  /**
   * 
   * @return a boolean indicating if the reference point is on the interior of the closed path
   *         represented by this plotter
   */
  boolean isReferencePointInterior();



  /**
   * Retrieves a point either in the exterior or the interior of the closed path represented by this
   * plotter.
   * 
   */
  Point2D getReferencePoint(Point2D buffer);


  /**
   * Retrieves a point either in the exterior or the interior of the closed path represented by this
   * plotter.
   * 
   * @param buffer a buffer to receive the result
   * 
   * @return a reference to the supplied buffer for conveninece of method chaining
   */
  default Point2D getReferencePoint() {
    return getReferencePoint(new Point2D.Double());
  }

  /**
   * {@inheritDoc}
   * <p>
   * Since the path is closed, the following should be true in general:
   * 
   * <pre>
   *    plot(getDomain().getBegin(), new Point2D.Double()).equals(plot(getDomain().getEnd(), new Point2D.Double())
   * </pre>
   * 
   * which basically asserts that the plotted point at the beginning and end of the domain is the
   * same.
   * </p>
   */
  @Override
  UnwritableInterval getDomain();



}
