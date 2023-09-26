package picante.math.cones;

import static com.google.common.base.Preconditions.checkNotNull;

import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

/**
 * Package private enumeration that captures the various types of path closures that can happen when
 * dealing with simple cylindrical projections of the entire body (2*PI radian longitude range).
 */
enum SimpleCylindricalPathType {

  /**
   * Indicates the path to close contains the north pole, but requires a shift to the left to close
   * properly.
   */
  NORTH_POLE_LEFT_SHIFT(true) {

    @Override
    void close(Path2D path, Point2D start, Point2D end, double branchRightShift,
        double overPoleBorderPadding) {
      shiftAndCloseOverPole(path, end.getY(), start.getY(), -branchRightShift,
          end.getX() + overPoleBorderPadding,
          start.getX() - branchRightShift - overPoleBorderPadding, -2 * overPoleBorderPadding);
    }
  },

  /**
   * Indicates the path to close contains the north pole and requires no shift to close.
   */
  NORTH_POLE_NO_SHIFT(true) {

    @SuppressWarnings("unused")
    @Override
    void close(Path2D path, Point2D start, Point2D end, double branchRightShift,
        double overPoleBorderPadding) {

      /*
       * If the path moves left to right
       */
      if (end.getX() > start.getX()) {
        closeOverPole(path, end.getY(), start.getY(), 1 + overPoleBorderPadding,
            -overPoleBorderPadding, -2 * overPoleBorderPadding);
      } else {
        closeOverPole(path, end.getY(), start.getY(), -overPoleBorderPadding,
            1 + overPoleBorderPadding, -2 * overPoleBorderPadding);
      }
    }
  },

  /**
   * Indicates the path to close contains the north pole, but requires a shift to the right to close
   * properly.
   */
  NORTH_POLE_RIGHT_SHIFT(true) {

    @Override
    void close(Path2D path, Point2D start, Point2D end, double branchRightShift,
        double overPoleBorderPadding) {
      shiftAndCloseOverPole(path, end.getY(), start.getY(), branchRightShift,
          end.getX() - overPoleBorderPadding,
          start.getX() + branchRightShift + overPoleBorderPadding, -2 * overPoleBorderPadding);
    }
  },


  /**
   * Indicates the second path to join requires a shift to the left by the branch offset.
   */
  LEFT_SHIFT(false) {

    @SuppressWarnings("unused")
    @Override
    void close(Path2D path, Point2D start, Point2D end, double branchRightShift,
        double overPoleBorderPadding) {
      path.closePath();
      path.append(createShiftedPath(path, -branchRightShift, 0.0), false);
    }
  },

  /**
   * Indicates the second path to join requires no shift.
   */
  NO_SHIFT(false) {

    @SuppressWarnings("unused")
    @Override
    void close(Path2D path, Point2D start, Point2D end, double branchRightShift,
        double overPoleBorderPadding) {
      path.closePath();
    }
  },

  /**
   * Indicates the second path to join requires a shift to the right by the branch offset.
   */
  RIGHT_SHIFT(false) {

    @SuppressWarnings("unused")
    @Override
    void close(Path2D path, Point2D start, Point2D end, double branchRightShift,
        double overPoleBorderPadding) {
      path.closePath();
      path.append(createShiftedPath(path, branchRightShift, 0.0), false);
    }
  },

  /**
   * Indicates the path to close contains the south pole, but requires a shift to the left to close
   * properly.
   */
  SOUTH_POLE_LEFT_SHIFT(true) {

    @Override
    void close(Path2D path, Point2D start, Point2D end, double branchRightShift,
        double overPoleBorderPadding) {
      shiftAndCloseOverPole(path, end.getY(), start.getY(), -branchRightShift,
          end.getX() + overPoleBorderPadding,
          start.getX() - branchRightShift - overPoleBorderPadding, 1 + 2 * overPoleBorderPadding);
    }
  },

  /**
   * Indicates the path to close contains the south pole, but requires no shift to close properly.
   */
  SOUTH_POLE_NO_SHIFT(true) {

    @SuppressWarnings("unused")
    @Override
    void close(Path2D path, Point2D start, Point2D end, double branchRightShift,
        double overPoleBorderPadding) {
      /*
       * If the path moves from left to right
       */
      if (end.getX() > start.getX()) {
        closeOverPole(path, end.getY(), start.getY(), 1 + overPoleBorderPadding,
            -overPoleBorderPadding, 1 + 2 * overPoleBorderPadding);
      } else {
        closeOverPole(path, end.getY(), start.getY(), -overPoleBorderPadding,
            1 + overPoleBorderPadding, 1 + 2 * overPoleBorderPadding);
      }
    }
  },

  /**
   * Indicates the path to close contains the south pole, but requires a shift to the right to close
   * properly.
   */
  SOUTH_POLE_RIGHT_SHIFT(true) {

    @Override
    void close(Path2D path, Point2D start, Point2D end, double branchRightShift,
        double overPoleBorderPadding) {
      shiftAndCloseOverPole(path, end.getY(), start.getY(), branchRightShift,
          end.getX() - overPoleBorderPadding,
          start.getX() + branchRightShift + overPoleBorderPadding, 1 + 2 * overPoleBorderPadding);
    }
  };

  private final boolean isPolar;

  private SimpleCylindricalPathType(boolean isPolar) {
    this.isPolar = isPolar;
  }

  /**
   * Determines whether the path join type contains a pole or not.
   * 
   * @return true if the type contains either pole
   */
  boolean isPolar() {
    return isPolar;
  }

  abstract void close(Path2D path, Point2D start, Point2D end, double branchRightShift,
      double overPoleBorderPadding);

  /**
   * Closes a path over the pole by drawing a padded rectangular box around the outside of the
   * projection.
   * <p>
   * This is a simple utility method to consolidate the execution of the same code with different
   * parameters in the pole shift combinations.
   * </p>
   * 
   * <pre>
   *     ============================================================================<=====+--outerLat
   *                                                                                       |
   *                                                                                       |
   *     --------+------------------------------------------------+--------   Math.PI/2.0  |
   *             |                                                |                        ^
   *             |                                                |                        |
   *             |                                                |                        |              
   *      . . . .+ . . . . . . .o--------------------->-----------+--------------x===>=====+--firstLat
   *             |                                                |                        |
   *             |                                                |                    firstLon
   *             |                                                |    
   *             |                                                |
   *     --------+------------------------------------------------+--------  -Math.PI/2.0
   *       
   *       lowerBranchValue                      lowerBranchValue + (2 * Math.PI)
   * </pre>
   * 
   * @param path the path to close over the pole.
   * @param firstLat the latitude of the first point in the enclosing box (generally path's end
   *        point y-value)
   * @param lastLat the latitude of the last point in the enclosing box (generally path's start
   *        point y-value)
   * @param branchShift the signed branch shift direction
   * @param firstLon the longitude of the first padded point (usually path's end point x-value with
   *        a padding)
   * @param lastLon the longitude of the last padded point (usually path's start point x-value with
   *        a padding applied along with a branch shift)
   * @param outerLat the padded value of latitude
   */
  static void shiftAndCloseOverPole(Path2D path, double firstLat, double lastLat,
      double branchShift, double firstLon, double lastLon, double outerLat) {

    /*
     * Append the shifted path to the start of the supplied path.
     */
    Path2D shifted = createShiftedPath(path, branchShift, 0);
    shifted.append(path, true);
    path.reset();
    path.append(shifted, false);

    /*
     * Draw the box outside of the projection.
     */
    path.lineTo(firstLon, firstLat);
    path.lineTo(firstLon, outerLat);
    path.lineTo(lastLon, outerLat);
    path.lineTo(lastLon, lastLat);

    path.closePath();

  }

  /**
   * Closes a path over the pole without any shifting.
   * 
   * @param path the open path
   * @param firstLat the latitude associated with the first closure point
   * @param lastLat the latitude associated with the last closure point
   * @param firstLon the longitude associated with the first closure point
   * @param lastLon the longitude associated with the last closure point
   * @param outerLat the latitude of the over the pole value to utilize
   */
  static void closeOverPole(Path2D path, double firstLat, double lastLat, double firstLon,
      double lastLon, double outerLat) {
    path.lineTo(firstLon, firstLat);
    path.lineTo(firstLon, outerLat);
    path.lineTo(lastLon, outerLat);
    path.lineTo(lastLon, lastLat);
    path.closePath();
  }


  /**
   * Identifies the type of a closed path.
   * 
   * @param path a closed path that has been enumerated from start to end
   * @param overPoleBranchTestTolerance
   * @param branchLeft
   * @param branchRight
   * 
   * @return the type of path for which closure is to be performed.
   */
  static SimpleCylindricalPathType identifyType(Path2D path, Point2D start, Point2D end,
      double overPoleBranchTestTolerance, double branchLeft, double branchRight) {

    /*
     * Determine if the region contains either pole. If it does, then it will have a single branch
     * path that does not "close" back onto itself; despite the fact it is actually closed.
     */
    if (Math.abs(end.getX() - start.getX()) > overPoleBranchTestTolerance) {

      /*
       * Examine the start of the path to determine which pole it is closer to. If it's exactly 0.5,
       * assume it's enclosing the south pole.
       * 
       * TODO: Consider alternatives, as this isn't really robust.
       */
      if (start.getY() < 0.5) {
        if (end.getX() < branchLeft) {
          return SimpleCylindricalPathType.NORTH_POLE_RIGHT_SHIFT;
        } else if (end.getX() > branchRight) {
          return SimpleCylindricalPathType.NORTH_POLE_LEFT_SHIFT;
        } else {
          return SimpleCylindricalPathType.NORTH_POLE_NO_SHIFT;
        }
      }

      /*
       * Handle the south pole cases.
       */
      if (end.getX() < branchLeft) {
        return SimpleCylindricalPathType.SOUTH_POLE_RIGHT_SHIFT;
      } else if (end.getX() > branchRight) {
        return SimpleCylindricalPathType.SOUTH_POLE_LEFT_SHIFT;
      } else {
        return SimpleCylindricalPathType.SOUTH_POLE_NO_SHIFT;
      }

    }

    /*
     * It's not a polar situation, examine the bounding rectangle to determine which type of shift
     * is necessary.
     */
    Rectangle2D bounds = path.getBounds2D();

    if (bounds.getMinX() < branchLeft) {
      return SimpleCylindricalPathType.RIGHT_SHIFT;
    }
    if (bounds.getMaxX() > branchRight) {
      return SimpleCylindricalPathType.LEFT_SHIFT;
    }
    return SimpleCylindricalPathType.NO_SHIFT;

  }

  /**
   * Creates a shifted version of the supplied path coordinates by the specified deltas in X and Y.
   * <p>
   * This is different than {@link AffineTransform#createTransformedShape(java.awt.Shape)} because
   * it returns a Path2D with the translated coordinates, instead of {@link Shape}.
   * </p>
   */
  static Path2D createShiftedPath(Path2D path, double deltaX, double deltaY) {

    checkNotNull(path);

    AffineTransform transform = AffineTransform.getTranslateInstance(deltaX, deltaY);

    /*
     * Use Path2D.Double, unless the supplied path is Path2D.Float.
     */
    if (path instanceof Path2D.Float) {
      return new Path2D.Float(path, transform);
    }
    return new Path2D.Double(path, transform);

  }

}
