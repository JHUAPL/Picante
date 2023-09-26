package picante.math.cones;

import java.awt.Graphics;
import java.awt.Shape;
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;

/**
 * 
 * Interface that describes an invertible map projection that takes a location on a body expressed
 * in latitude and longitude and converts it to the 2D projection coordinate space. The interface
 * also provides the inverse.
 * <p>
 * This API &quot;abuses&quot; the {@link Point2D} class as a vehicle for carrying longitude (x
 * component) and latitude (y component). This was done intentionally to stick with the utilization
 * of the {@link Graphics} classes for rendering.
 * </p>
 * 
 * An interface that provides methods designed to work with open and closed parameterized paths in
 * longitude and latitude into map projected space. The {@link PathPlotter} interface is used by
 * these implementations in a specific way, as it is considered to be providing a curve
 * parameterized in the longitude-latitude angle space.
 * <p>
 * The reason the {@link PathPlotter} interface is utilized here instead of a collection of discrete
 * sampled points, is that this allows the map projection to decide to refine the step-size in the
 * event that projection artifacts require an increased sampling in a region. If you are working
 * with a discrete set of points, then take a look at
 * {@link PathPlotters#create(crucible.core.data.list.indexable.Indexable)}.
 * </p>
 * <p>
 * Path plotters consumed by the implementations of this interface are expected to supply longitude
 * as the x-coordinate of the point, and latitude as the y-coordinate of the point. Further, they
 * may assume that longitudes must be confined to the range [-Pi,Pi], and latitudes should be
 * confined to the range: [-Pi/2, Pi/2].
 * </p>
 * 
 */
interface Projection {

  /**
   * Projects a supplied longitude and latitude into the map projected space.
   * <p>
   * Implementors may rely upon longitude lying in the range: [-Math.PI, Math.PI] and latitude in
   * the range: [-Math.PI/2.0, Math.PI/2.0]
   * </p>
   * 
   * @param latLon longitude and latitude in radians
   * @param buffer buffer to receive the results of the projection
   * 
   * @return a reference to the supplied buffer for convenience in chaining method calls
   */
  public <P extends Point2D> P project(Point2D latLon, P buffer);

  default Point2D project(Point2D latLon) {
    return project(latLon, new Point2D.Double());
  }

  /**
   * Takes a point expressed in the projection and inverts it back into longitude and latitude.
   * 
   * @param mapLocation the location in the map projected coordinates
   * @param buffer buffer to receive the results of the inversion in longitude latitude coordinates.
   * 
   * @return a reference to the supplied buffer for convenience in chaining method calls
   * 
   * @throws InversionFailedException if the supplied mapLocation is unable to be inverted,
   *         typically because it lies outside {@link #getValidRegion()}.
   */
  public <P extends Point2D> P invert(Point2D mapLocation, P buffer);

  default Point2D invert(Point2D mapLocation) {
    return invert(mapLocation, new Point2D.Double());
  }

  /**
   * Returns the &quot;reference&quot; point of the projection, i.e. the point at which the map
   * projection distortion is a minimum. Generally this is the central point of the projection, but
   * it may not be the case always.
   * 
   * @return a point in normalized coordinates; it may lie outside the [0,0]x[1,1] bounding box.
   *         This should be a newly created instance of Point2D every time this method is invoked
   *         for safety reasons.
   */
  public Point2D getReference();

  /**
   * Returns the native aspect ratio of the projected space.
   * <p>
   * Since the projected space is a (0,1) x (0,1) normalized space, this method returns the ratio of
   * the width to the height.
   * </p>
   * 
   * @return the number of width units divided by the number of equivalent height units.
   */
  public double getNativeAspectRatio();

  /**
   * Returns the subset of the (0,1) x (0,1) normalized space that where the implementation will
   * project into.
   * <p>
   * Some projections, due to their nature, will not fill the normalized space entirely. This method
   * will give the boundary and interior of that region.
   * </p>
   * 
   * @return
   */
  public Shape getValidRegion();

  /**
   * Projects an open path specified in longitude and latitude into the map projected space.
   * 
   * @param plotter the path plotter providing the parameterized function generating longitude and
   *        latitude of a path on the surface.
   * @param step the requested &quot;step-size&quot; used to generate points.
   * 
   * @return a reference to buffer, whose contents capture the open path projected into the map
   *         projected space. For map projections with branch cuts and other features, this may
   *         require multiple disconnected paths to properly capture.
   * 
   * @throws IllegalArgumentException if step is not strictly positive
   */
  public <R extends Path2D> R projectOpenPath(PathPlotter plotter, double step, R buffer);

  default Path2D projectOpenPath(PathPlotter plotter, double step) {
    return projectOpenPath(plotter, step, new Path2D.Double());
  }

  /**
   * Projects a closed path specified in longitude and latitude into the map projected space.
   * 
   * @param plotter the path plotter providing the parameterized function generating the longitude
   *        and latitude of a closed path on the surface.
   * @param step the requested &quot;step-size&quot; used to generate points.
   * @param buffer the Path2D to populate, any previously defined path content is dropped via a call
   *        to {@link Path2D#reset()} prior to populating additional points.
   * 
   * @return a reference to buffer, whose contents capture the closed path projected into the map
   *         projected space. For map projections with branch cuts and other features, this may
   *         require multiple, disconnected areas.
   * 
   * @throws IllegalArgumentException if step is not strictly positive
   */
  public <R extends Path2D> R projectClosedPath(ClosedPathPlotter plotter, double step, R buffer);

  default Path2D projectClosedPath(ClosedPathPlotter plotter, double step) {
    return projectClosedPath(plotter, step, new Path2D.Double());
  }

  /**
   * Certain classes of projections are derivatives (i.e. flipped, clipped, or rotated) of other
   * projections. This method returns the type of the original or &quot;source&quot; projection,
   * walking the hierarchy recursively if required.
   * <p>
   * Most implementations will simply return {@link Projection#getClass()}.
   * </p>
   * 
   * @return the type of the original projection
   */
  Class<? extends Projection> getOriginalClass();

  /**
   * Creates a new projection, wrapping the instance, where vectors derived from latitude/longitude
   * pairs are rotated prior to application of the projection.
   * 
   * @param rotation rotation from standard latitude/longitude, body-fixed coordinate system to the
   *        newly defined one.
   * 
   * @return new instance of the projection
   */
  Projection rotate(UnwritableRotationMatrixIJK rotation);

}
