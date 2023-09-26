package picante.math.cones;

import java.awt.geom.Point2D;
import picante.math.coords.CoordConverters;
import picante.math.coords.LatitudinalVector;
import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * Class that contains a variety of utility methods for use with {@link PathPlotters}.
 */
class PathPlotters {

  private PathPlotters() {}

  /**
   * Converts the supplied vector into a lon/lat Point2D
   */
  public static Point2D convert(UnwritableVectorIJK vector) {
    return convert(vector, new Point2D.Double());
  }

  /**
   * Converts the supplied vector into a lon/lat Point2D
   */
  public static Point2D convert(UnwritableVectorIJK vector, Point2D buffer) {
    LatitudinalVector latVec = CoordConverters.convertToLatitudinal(vector);
    buffer.setLocation(latVec.getLongitude(), latVec.getLatitude());
    return buffer;
  }

  /**
   * Rotates a {@link Point2D} containing a latitude and longitude from one frame to another in
   * place.
   * 
   * @param buffer the buffer containing the point prior to the rotation, and the rotated point
   *        after the method finishes execution
   * 
   * @param rotationalToNewFrame the matrix rotation from the frame in which buffer is initially
   *        expressed to an alternate one
   * 
   */
  static void rotatePointInPlace(Point2D buffer, UnwritableRotationMatrixIJK rotationalToNewFrame) {
    UnwritableVectorIJK vector =
        CoordConverters.convert(new LatitudinalVector(1.0, buffer.getY(), buffer.getX()));
    VectorIJK rotated = rotationalToNewFrame.mxv(vector);
    LatitudinalVector latLonNew = CoordConverters.convertToLatitudinal(rotated);
    buffer.setLocation(latLonNew.getLongitude(), latLonNew.getLatitude());
  }


  /**
   * Applies a rotation matrix to a path plotter to produce a new path plotter.
   * 
   * @param plotter the plotter to rotate
   * 
   * @param fromPlotterToNewFrame rotation from the supplied plotter's frame to a new frame; a copy
   *        of this matrix is retained at construction time
   * 
   * @return newly created, rotated {@link PathPlotter}
   */
  public static PathPlotter rotate(final PathPlotter plotter,
      final UnwritableRotationMatrixIJK fromPlotterToNewFrame) {
    return new PathPlotter() {

      private final UnwritableRotationMatrixIJK matrix =
          UnwritableRotationMatrixIJK.copyOf(fromPlotterToNewFrame);

      @Override
      public Point2D plot(double p, Point2D buffer) {
        plotter.plot(p, buffer);
        rotatePointInPlace(buffer, matrix);
        return buffer;
      }

      @Override
      public UnwritableInterval getDomain() {
        return plotter.getDomain();
      }
    };

  }
  
  /**
   * Applies a rotation matrix to a closed path plotter to produce a new one.
   * 
   * @param plotter the plotter to rotate
   * 
   * @param fromPlotterToNewFrame rotation from the supplied plotter's frame to a new frame; a copy
   *        of this matrix is retained at construction time
   * 
   * @return newly created, rotated {@link PathPlotter}
   */
  public static ClosedPathPlotter rotate(final ClosedPathPlotter plotter,
      final UnwritableRotationMatrixIJK fromPlotterToNewFrame) {

    return new ClosedPathPlotter() {

      private final UnwritableRotationMatrixIJK matrix =
          UnwritableRotationMatrixIJK.copyOf(fromPlotterToNewFrame);

      @Override
      public Point2D plot(double p, Point2D buffer) {
        plotter.plot(p, buffer);
        rotatePointInPlace(buffer, matrix);
        return buffer;
      }

      @Override
      public UnwritableInterval getDomain() {
        return plotter.getDomain();
      }

      @Override
      public boolean isReferencePointInterior() {
        return plotter.isReferencePointInterior();
      }

      @Override
      public Point2D getReferencePoint(Point2D buffer) {
        plotter.getReferencePoint(buffer);
        rotatePointInPlace(buffer, matrix);
        return buffer;
      }
    };
  }
}
