package picante.math.cones;

import static com.google.common.base.Preconditions.checkArgument;
import static picante.units.FundamentalPhysicalConstants.TWOPI;
import java.awt.Shape;
import java.awt.geom.Area;
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import picante.exceptions.BugException;
import picante.math.intervals.UnwritableInterval;

/**
 * Simple &quot;full sphere&quot; cylindrical projection implementation.
 * <p>
 * This class is public to enable identification of its projection type by standard code
 * introspection techniques, however it can only be created through the static methods implemented
 * on the {@link Projections} class.
 * </p>
 * <p>
 * This code is derived from a slightly more generic implementation of this algorithm, so there are
 * artifacts with regards to fields on the class that could be implementation constants. It was left
 * this way so that the more general implementation could be easily extracted if necessary in the
 * future.
 * </p>
 * <p>
 * TODO: Handle the multiple "wrapping" case. The current code when it unwinds a closed path does
 * not properly allow it to be rendered unless it only moves left or right across the branch cut
 * once.
 * </p>
 */
class SimpleCylindrical extends AbstractProjection {

  /**
   * The value of the left branch in radians. This is necessary since this code is not only
   * unraveling the branch cut wrapping, but performing the affine transformation from radians space
   * to the normalized space.
   */
  private final double branchLeftInRadians;

  /**
   * The numeric value at which the left branch is defined. Note: in this implementation this value
   * is always 0.0.
   */
  private final double branchLeft;

  /**
   * The numeric value at which the right branch is defined. Note: in this implementation this value
   * is always 1.0.
   */
  private final double branchRight;

  /**
   * The value at which a step in a point plotter must exceed to be considered a branch crossing.
   * This value should be something approximately 0.5 in this implementation for most applications.
   */
  private final double split;

  /**
   * The mid-point between the two branches, normally would be the average of branchLeft and
   * branchRight which in this implementation is 0.5.
   */
  private final double midPoint;

  /**
   * The amount to shift by numerically when moving a point from one branch to another towards the
   * right branch. In this implementation it is 1.0 (branchRight - branchLeft).
   */
  private final double branchRightShift;

  /**
   * The amount of horizontal (longitude) separation required between the first and final point of a
   * closed path to be considered &quot;open&quot; and thus enclosing the pole.
   */
  private final double overPoleBranchTestTolerance = (TWOPI - 0.01) / TWOPI;

  /**
   * The amount of padding to add to tracks that enclose the pole.
   */
  private final double overPoleBorderPadding;

  /**
   * Package private constructor.
   * 
   * @param branchLeftInRadians
   * @param split
   */
  SimpleCylindrical(double branchLeftInRadians, double split, double overPoleBorderPadding) {
    super();

    checkArgument(branchLeftInRadians >= -TWOPI);
    checkArgument(branchLeftInRadians <= 0.0);

    checkArgument(split > 0);
    checkArgument(split < TWOPI);

    checkArgument(overPoleBorderPadding > 0);

    this.branchLeftInRadians = branchLeftInRadians;
    this.branchLeft = 0.0;
    this.branchRight = 1.0;
    this.split = split / TWOPI;
    this.midPoint = 0.5;
    this.branchRightShift = branchRight - branchLeft;
    this.overPoleBorderPadding = overPoleBorderPadding / TWOPI;
  }

  @Override
  public <P extends Point2D> P project(Point2D latLon, P buffer) {

    /*
     * Assuming the latitude and longitude lie in the appropriate ranges:
     */
    double y = -1.0 / Math.PI * latLon.getY() + 0.5;

    double longitude = latLon.getX() - branchLeftInRadians;

    if (longitude < 0) {
      longitude += TWOPI;
    } else if (longitude > TWOPI) {
      longitude -= TWOPI;
    }

    double x = longitude / TWOPI;

    buffer.setLocation(x, y);

    return buffer;
  }

  @Override
  public <P extends Point2D> P invert(Point2D mapLocation, P buffer) {

    double latitude = -Math.PI * (mapLocation.getY() - 0.5);

    double longitude = mapLocation.getX() * TWOPI + branchLeftInRadians;

    if (longitude < -Math.PI) {
      longitude += TWOPI;
    } else if (longitude > TWOPI) {
      longitude -= TWOPI;
    }

    buffer.setLocation(longitude, latitude);

    return buffer;
  }

  /**
   * This method exists as the original code did not also perform the affine transformation scaling
   * the latitude and longitude to the normalized space.
   */
  private PathPlotter transformToNormalizedCoordinates(final PathPlotter plotter) {
    return new PathPlotter() {

      @Override
      public Point2D plot(double p, Point2D buffer) {

        /*
         * Plot the latLon based point.
         */
        plotter.plot(p, buffer);

        /*
         * Convert the point to the appropriate range.
         */
        project(buffer, buffer);

        return buffer;
      }

      @Override
      public UnwritableInterval getDomain() {
        return plotter.getDomain();
      }

    };
  }

  @Override
  public <R extends Path2D> R projectOpenPath(PathPlotter plotter, double step, R buffer) {

    /*
     * This code could utilize the createSingleBranchPath method utilized in the projectClosedPath
     * method in much the same manner. However, this would result in lots of redundant points along
     * the path. So this method skips those points in the way it is implemented.
     */

    checkArgument(step > 0, "Step argument must be greater than zero.");

    /*
     * Reset the supplied buffer to dump all of the coordinates and segments it possesses.
     */
    buffer.reset();

    /*
     * Adapt the supplied plotter into normalized coordinates before wrapping.
     */
    plotter = transformToNormalizedCoordinates(plotter);

    /*
     * Consider returning multiple paths, that cover both edges of the branch cut.
     */
    Point2D.Double prevPoint = new Point2D.Double();
    Point2D.Double point = new Point2D.Double();

    /*
     * Setup the iteration of the point plotter. Start at the origin of the specified domain.
     */
    double t = plotter.getDomain().getBegin();
    plotter.plot(t, point);

    /*
     * Take the first "step", contract to the end of the domain if it oversteps.
     */
    int i = 1;
    t = Math.min(plotter.getDomain().getBegin() + step, plotter.getDomain().getEnd());

    /*
     * Check to see if the starting point is exactly one of the poles. If it is then peek ahead to
     * the next point. We will set the longitude of the point (which is meaningless in general,
     * except in this projection) to match that value.
     */
    if ((point.y == 0.0) || (point.y == 1.0)) {
      plotter.plot(t, prevPoint);
      point.x = prevPoint.x;
    }

    /*
     * Move to the start of the path, and set the previous point to the path starting location.
     */
    buffer.moveTo(point.x, point.y);
    prevPoint.setLocation(point);

    while (t <= plotter.getDomain().getEnd()) {

      plotter.plot(t, point);

      /*
       * Check to see if point is the pole. If it is then adjust the longitude to match that of the
       * previous point.
       */
      if ((point.y == 0.0) || (point.y == 1.0)) {
        point.x = prevPoint.x;
      }

      /*
       * Check to see if the separation between this point and the next exceeds split.
       */
      if (Math.abs(point.x - prevPoint.x) > split) {

        /*
         * If point.x is closer to branchLeft, then adjust it to the right.
         */
        if (point.x < midPoint) {
          buffer.lineTo(point.x - branchLeft + branchRight, point.y);
          buffer.moveTo(branchLeft - branchRight + prevPoint.x, prevPoint.y);
          buffer.lineTo(point.x, point.y);

        } else {
          buffer.lineTo(branchLeft - branchRight + point.x, point.y);
          buffer.moveTo(prevPoint.x - branchLeft + branchRight, prevPoint.y);
          buffer.lineTo(point.x, point.y);
        }

      } else {
        buffer.lineTo(point.x, point.y);
      }

      /*
       * Prepare to enter the next iteration. First check to see if we are on the final point of the
       * iteration. If we are, then simply return the path we've been creating. Otherwise continue
       * on.
       */
      if (t == plotter.getDomain().getEnd()) {
        return buffer;
      }

      /*
       * Update prevPoint, i, and t for the next iteration.
       */
      prevPoint.setLocation(point);
      i++;
      t = Math.min(plotter.getDomain().getBegin() + i * step, plotter.getDomain().getEnd());

    }

    /*
     * We should never reach here if the iteration through the steps works properly.
     */
    throw new BugException("Failure in projecting open path.  This should never happen and "
        + "is indicative of a failure in the projection algorithm.");
  }

  @Override
  public <R extends Path2D> R projectClosedPath(ClosedPathPlotter plotter, double step, R buffer) {


    checkArgument(step > 0, "Step argument must be greater than zero.");

    /*
     * Create a boundary path on a single branch. If the closed path contains the pole, then the
     * path will start and end approximately 2*PI apart.
     */
    createSingleBranchPath(plotter, plotter.getDomain(), step, buffer);

    Point2D end = buffer.getCurrentPoint();
    Point2D start = getPathStart(buffer);

    /*
     * Won't work for paths that cross more than one branch in the same direction (spiral around the
     * sphere)
     */
    SimpleCylindricalPathType
        .identifyType(buffer, start, end, overPoleBranchTestTolerance, branchLeft, branchRight)
        .close(buffer, start, end, branchRightShift, overPoleBorderPadding);
    /*
     * Test if the reference point is an interior point, by projecting it
     */
    boolean isReferencePointInterior = plotter.isReferencePointInterior();
    Point2D referencePt = plotter.getReferencePoint();
    Point2D projectedReferencePt = project(referencePt, new Point2D.Double());
    /*
     * Render the inside of the buffer if the reference point is contained and is interior, or if it
     * is not contained and it is not interior.
     */
    if (buffer.contains(projectedReferencePt) == isReferencePointInterior) {
      /*
       * This is an abuse of the accumulator implementation, but it's more efficient than performing
       * the clone under the build() method.
       */
      return buffer;
    }


    Rectangle2D.Double entireShape = new Rectangle2D.Double(-overPoleBorderPadding,
        -2 * overPoleBorderPadding, 1 + 2 * overPoleBorderPadding, 1 + 4 * overPoleBorderPadding);

    Area entireShapeArea = new Area(entireShape);
    entireShapeArea.subtract(new Area(buffer));

    buffer.reset();
    buffer.append(entireShapeArea, false);

    return buffer;
  }

  /**
   * Creates a {@link Path2D} from the supplied plotter in a simple cylindrical projection across
   * multiple branches as necessary.
   */
  private <R extends Path2D> R createSingleBranchPath(PathPlotter plotter,
      UnwritableInterval domain, double step, R buffer) {

    /*
     * Dump the contents of buffer.
     */
    buffer.reset();

    /*
     * Adapt the supplied plotter into normalized coordinates before wrapping.
     */
    plotter = transformToNormalizedCoordinates(plotter);

    checkArgument(!domain.isSingleton(),
        "Domain over which plotting occurs must have non-zero support.");

    double branchAdjust = 0;

    Point2D.Double prevPoint = new Point2D.Double();
    Point2D.Double point = new Point2D.Double();

    /*
     * Setup the iteration of the point plotter. Start at the origin of the specified domain.
     */
    double t = domain.getBegin();
    plotter.plot(t, point);

    /*
     * Prepare to take the first "step", constrain to the end of the domain if it over steps.
     */
    int i = 1;
    t = Math.min(domain.getBegin() + step, domain.getEnd());

    /*
     * Check to see if the starting point is exactly one of the poles. If it is then peek ahead to
     * the next point. We will set the longitude of the point (which is meaningless in general,
     * except in this projection) to match that value.
     */
    if ((point.y == 0.0) || (point.y == 1.0)) {
      plotter.plot(t, prevPoint);
      point.x = prevPoint.x;
    }

    /*
     * Move to the start of the path, and set the previous point to the path starting location.
     */
    buffer.moveTo(point.x, point.y);
    prevPoint.setLocation(point);

    while (t <= domain.getEnd()) {

      plotter.plot(t, point);

      /*
       * Apply the current branch adjustment to point.x.
       */
      point.x += branchAdjust;

      /*
       * Check to see if point is the pole, if it is then adjust the longitude to match that of the
       * previous point. This check is made after, since prevPoint has already had it's branch
       * adjustment made in the previous iteration.
       */
      if ((point.y == 0.0) || (point.y == 1.0)) {
        point.x = prevPoint.x;
      }

      /*
       * Now check to see if the separation between this point and the next exceeds split.
       */
      if (Math.abs(point.x - prevPoint.x) > split) {

        /*
         * If point.x is closer to branchLeft, then adjust it to the right.
         */
        if (point.x < midPoint) {
          branchAdjust += branchRight - branchLeft;
          point.x += branchRight - branchLeft;
        } else {
          branchAdjust -= branchRight - branchLeft;
          point.x -= branchRight - branchLeft;
        }

      }

      buffer.lineTo(point.x, point.y);

      /*
       * Prepare to enter the next iteration. First check to see if we are on the final point of the
       * iteration. If we are, then simply return the path we've been creating. Otherwise continue
       * on.
       */
      if (t == domain.getEnd()) {
        return buffer;
      }

      prevPoint.setLocation(point);

      i++;

      /*
       * Update t to the next step, constrain it to the end of the domain.
       */
      t = Math.min(domain.getBegin() + i * step, domain.getEnd());
    }

    /*
     * We should never reach here if the iteration through the steps works properly.
     */
    throw new BugException("Unable to project shape.  This should never happen and is "
        + "indicative of a failure in the algorithm.");

  }

  static Point2D getPathStart(Path2D path) {
    Point2D result = path.getCurrentPoint();
    double[] segment = new double[6];
    path.getPathIterator(null).currentSegment(segment);
    result.setLocation(segment[0], segment[1]);
    return result;
  }


  @Override
  public double getNativeAspectRatio() {
    return 2.0;
  }

  @Override
  public Shape getValidRegion() {
    return new Rectangle2D.Double(0, 0, 1, 1);
  }

  @Override
  public Point2D getReference() {
    return new Point2D.Double(0.5, 0.5);
  }

}
