package picante.surfaces;

import static com.google.common.base.Preconditions.checkArgument;
import picante.designpatterns.Writable;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.surfaces.Ellipsoid.PointAndDistance;

/**
 * A class capturing an ellipse in a 3D vector space.
 */
public class Ellipse extends UnwritableEllipse implements Writable<UnwritableEllipse, Ellipse> {

  /**
   * Enumeration describing the type of ellipses supported by this class.
   */
  public enum Type {

    /**
     * Indicates the ellipse has both semi-axes with zero length.
     */
    POINT(true) {

      @Override
      boolean intersects(UnwritableEllipse ellipse, UnwritablePlane plane) {

        checkArgument(ellipse.type == POINT);

        /*
         * This is simple, determine if the point is contained within the plane.
         */
        return plane.contains(ellipse.center);
      }

      @Override
      boolean isContainedWithin(UnwritableEllipse ellipse, UnwritablePlane plane) {

        checkArgument(ellipse.type == POINT);

        /*
         * Also simple, just determine if the point is contained with the plane.
         */
        return plane.contains(ellipse.center);
      }

      @Override
      void intersect(UnwritableEllipse ellipse, UnwritablePlane plane, VectorIJK bufferOne,
          VectorIJK bufferTwo) {
        checkArgument(ellipse.type == POINT);
        checkArgument(isContainedWithin(ellipse, plane),
            "Point degenerate ellipse does not intersect plane");
        bufferOne.setTo(ellipse.center);
        bufferTwo.setTo(ellipse.center);
      }

      @Override
      PointAndDistance computeNearPoint(UnwritableEllipse ellipse, VectorIJK position) {
        throw new UnsupportedOperationException("Not yet implemented.");
      }

    },

    /**
     * Indicates the ellipse has a semi-minor axes with zero length.
     */
    LINE_SEGMENT(true) {

      @Override
      boolean intersects(UnwritableEllipse ellipse, UnwritablePlane plane) {

        checkArgument(ellipse.type == LINE_SEGMENT);
        /*
         * First get the entire containment case out of the way.
         */
        if (isContainedWithin(ellipse, plane)) {
          return true;
        }

        /*
         * Compute the plane constant after translating the origin so that it is at the ellipse's
         * center.
         */
        VectorIJK recenteredOrigin =
            VectorIJK.combine(plane.getConstant(), plane.normal, -1.0, ellipse.center);
        double recenteredConstant = Math.abs(plane.normal.getDot(recenteredOrigin));

        return Math.abs(ellipse.smajor.getDot(plane.normal)) >= recenteredConstant;
      }

      @Override
      boolean isContainedWithin(UnwritableEllipse ellipse, UnwritablePlane plane) {
        checkArgument(ellipse.type == LINE_SEGMENT);
        /*
         * Determine if the center is contained within the plane.
         */
        if (!plane.contains(ellipse.center)) {
          return false;
        }

        /*
         * Check that the semi-major axes is orthogonal to the the plane's normal.
         */
        return ellipse.smajor.getDot(plane.normal) == 0;
      }

      @Override
      void intersect(UnwritableEllipse ellipse, UnwritablePlane plane, VectorIJK bufferOne,
          VectorIJK bufferTwo) {

        checkArgument(ellipse.type == LINE_SEGMENT);

        /*
         * Verify that the line segment is not completely contained within the plane.
         */
        checkArgument(!isContainedWithin(ellipse, plane),
            "Line segment degenerate ellipse is entirely " + "contained within the plane");

        /*
         * We will handle the case of no intersection as we go forward. While we could utilize the
         * intersects() method on this type, it would result in redundant execution. Create a new
         * plane that is just the supplied plane recentered to the center of the ellipse.
         */
        VectorIJK newCenter =
            VectorIJK.combine(plane.getConstant(), plane.normal, -1.0, ellipse.center);
        Plane newPlane = new Plane(plane.normal, newCenter);

        double v = ellipse.smajor.getDot(newPlane.normal);
        double absV = Math.abs(v);

        /*
         * Check to see if there is no intersection. This is two parts, first if the line segment is
         * parallel to the plane (v == 0) or if Math.abs(v) < newPlane.getConstant.
         */
        checkArgument(v != 0, "Line segment degenerate ellipse is parallel to"
            + " the candidate plane for intersection");
        checkArgument(absV >= newPlane.getConstant(),
            "Line segment degenerate ellipse does not " + "intersect the candidate plane.");

        /*
         * The sign of the scale for the semi-major axis should be the same sign as v.
         */
        VectorIJK.combine(1.0, ellipse.center, Math.signum(v) * newPlane.getConstant() / absV,
            ellipse.smajor, bufferOne);
        bufferTwo.setTo(bufferOne);

      }

      @Override
      PointAndDistance computeNearPoint(UnwritableEllipse ellipse, VectorIJK position) {
        // TODO
        throw new UnsupportedOperationException("Not yet implemented.");
      }

    },

    /**
     * Indicates the ellipse has a semi-axes with non-zero length.
     */
    ELLIPSE(false) {

      @Override
      boolean intersects(UnwritableEllipse ellipse, UnwritablePlane plane) {

        checkArgument(ellipse.type == ELLIPSE);

        /*
         * First get the entire containment case out of the way.
         */
        if (isContainedWithin(ellipse, plane)) {
          return true;
        }

        /*
         * Compute the plane constant after translating the origin so that it is at the ellipse's
         * center.
         */
        VectorIJK recenteredOrigin =
            VectorIJK.combine(plane.getConstant(), plane.normal, -1.0, ellipse.center);
        double recenteredConstant = Math.abs(plane.normal.getDot(recenteredOrigin));

        double v1 = ellipse.smajor.getDot(plane.normal);
        double v2 = ellipse.sminor.getDot(plane.normal);

        return Math.hypot(v1, v2) >= recenteredConstant;

      }

      @Override
      boolean isContainedWithin(UnwritableEllipse ellipse, UnwritablePlane plane) {

        checkArgument(ellipse.type == ELLIPSE);

        /*
         * Determine if the center is contained within the plane.
         */
        if (!plane.contains(ellipse.center)) {
          return false;
        }

        /*
         * Verify that both semi-axes are orthogonal to the plane's normal.
         */
        return ((ellipse.smajor.getDot(plane.normal) == 0)
            && (ellipse.sminor.getDot(plane.normal) == 0));
      }

      @Override
      void intersect(UnwritableEllipse ellipse, UnwritablePlane plane, VectorIJK bufferOne,
          VectorIJK bufferTwo) {

        checkArgument(ellipse.type == ELLIPSE);

        /*
         * Verify that the line segment is not completely contained within the plane.
         */
        checkArgument(!isContainedWithin(ellipse, plane),
            "Ellipse is entirely contained within the plane");

        /*
         * We will handle the case of no intersection as we go forward. While we could utilize the
         * intersects() method on this type, it would result in redundant execution. Create a new
         * plane that is just the supplied plane recentered to the center of the ellipse.
         */
        VectorIJK newCenter =
            VectorIJK.combine(plane.getConstant(), plane.normal, -1.0, ellipse.center);
        Plane newPlane = new Plane(plane.normal, newCenter);

        double v1 = ellipse.smajor.getDot(newPlane.normal);
        double v2 = ellipse.sminor.getDot(newPlane.normal);

        double vnorm = Math.hypot(v1, v2);

        /*
         * Check to see if there is no intersection. This is two parts, first if the line segment is
         * parallel to the plane (v == 0) or if Math.abs(v) < newPlane.getConstant.
         */
        checkArgument((v1 != 0.0) || (v2 != 0.0),
            "Ellipse is parallel to the candidate plane for intersection");
        checkArgument(vnorm >= newPlane.getConstant(),
            "Ellipse does not intersect the candidate plane");

        double alpha = Math.acos(newPlane.getConstant() / vnorm);
        double beta = Math.atan2(v2, v1);

        double angle1 = beta - alpha;
        double angle2 = beta + alpha;

        VectorIJK.combine(1.0, ellipse.center, Math.cos(angle1), ellipse.smajor, Math.sin(angle1),
            ellipse.sminor, bufferOne);

        VectorIJK.combine(1.0, ellipse.center, Math.cos(angle2), ellipse.smajor, Math.sin(angle2),
            ellipse.sminor, bufferTwo);
      }

      @Override
      PointAndDistance computeNearPoint(UnwritableEllipse ellipse, VectorIJK position) {
        checkArgument(ellipse.type == ELLIPSE);

        /*
         * find the lengths of the semi-axes and scale the vectors
         */
        double minLength = ellipse.getSemiMinorAxis().getLength();
        double majLength = ellipse.getSemiMajorAxis().getLength();


        double scale = 1. / majLength;
        VectorIJK sMajor = new VectorIJK(scale, ellipse.getSemiMajorAxis());
        VectorIJK sMinor = new VectorIJK(scale, ellipse.getSemiMinorAxis());

        /*
         * translate ellipse and point so that the ellipse is centered at the origin
         */
        VectorIJK tmppnt = VectorIJK.subtract(position, ellipse.getCenter());
        tmppnt.scale(scale);

        /*
         * reduce to 2 dimensions. Work in a coord system whose x and y axes are aligned with the
         * semi major and semi minor axes of the ellipse
         */
        // TODO note this is a quick implementation of twovec, but twovec should be implemented
        // somewhere in core. when it is, this should be replaced with that.
        VectorIJK i = sMajor;
        VectorIJK j = sMinor;
        VectorIJK k = VectorIJK.uCross(sMajor, sMinor);
        j.setTo(VectorIJK.uCross(k, i));
        RotationMatrixIJK rotate =
            new RotationMatrixIJK(i.createUnitized(), j, k).createTranspose();

        VectorIJK tmpv = rotate.mxv(tmppnt);
        tmppnt.setTo(tmpv);
        VectorIJK prjpnt = new VectorIJK(tmppnt.get(0), tmppnt.get(1), 0);

        /*
         * Find the nearest point to prjpnt on the ellipsoid, pnear.
         */
        Ellipsoid ellipsoid = new Ellipsoid(1., minLength / majLength, 2.);
        PointAndDistance pointAndDistance = ellipsoid.computeNearPoint(prjpnt);
        VectorIJK pNear = new VectorIJK(pointAndDistance.getPoint());

        /*
         * scale and translate the results, as necessary
         */
        pNear.scale(majLength);
        tmpv.setTo(rotate.mtxv(pNear));
        pNear.setTo(VectorIJK.add(tmpv, ellipse.getCenter()));

        double dist = pNear.getDistance(position);

        return new PointAndDistance(pNear, dist);
      }
    };

    /**
     * Field indicating whether the ellipse type is degenerate.
     */
    private final boolean degenerate;

    /**
     * Constructs the instance with the given degeneracy.
     * 
     * @param degenerate is the type degenerate?
     */
    private Type(boolean degenerate) {
      this.degenerate = degenerate;
    }

    /**
     * Indicates whether the type is degenerate or not.
     * 
     * @return true if degenerate, false otherwise.
     */
    public boolean isDegenerate() {
      return degenerate;
    }

    /**
     * Package private method that determines whether the ellipse of a particular type intersects a
     * plane.
     * <p>
     * Designed to support {@link UnwritableEllipse#intersects(UnwritablePlane)}
     * </p>
     * 
     * @param ellipse the ellipse
     * @param plane the plane
     * 
     * @return true if ellipse and plane intersect, false otherwise
     */
    abstract boolean intersects(UnwritableEllipse ellipse, UnwritablePlane plane);

    /**
     * Package private method that determines whether the ellipse of a particular type is completely
     * contained within a plane.
     * <p>
     * Designed to support {@link UnwritableEllipse#isContainedWithin(UnwritablePlane)}
     * </p>
     * 
     * @param ellipse the ellipse
     * @param plane the plane
     * 
     * @return true if plane completely contains ellipse, false otherwise
     */
    abstract boolean isContainedWithin(UnwritableEllipse ellipse, UnwritablePlane plane);

    /**
     * Computes the intersection points of an ellipse of a particular type with a plane.
     * <p>
     * Designed to support
     * {@link UnwritableEllipse#intersect(UnwritablePlane, VectorIJK, VectorIJK)}
     * </p>
     * 
     * @param ellipse the ellipse
     * @param plane the plane
     * @param bufferOne buffer to capture the an intersection point
     * @param bufferTwo another, separate buffer, to capture another intersection point
     * 
     * @throws IllegalArgumentException if there are no intersections or if the ellipse is
     *         completely contained within the plane. To guard against this use:
     *         {@link UnwritableEllipse#isContainedWithin(UnwritablePlane)} and
     *         {@link UnwritableEllipse#intersects(UnwritablePlane)}
     */
    abstract void intersect(UnwritableEllipse ellipse, UnwritablePlane plane, VectorIJK bufferOne,
        VectorIJK bufferTwo);

    /**
     * Computes the nearest point on an ellipse to a specified point, both in three-dimensional
     * space, and finds the distance between the ellipse and the point. This is an implementation of
     * SPICE's NPELPT. As it stands, this method throws an error if the type is a line or a point.
     * <p>
     * Designed to support {@link UnwritableEllipse#computeNearPoint(VectorIJK)}
     * 
     * 
     * @param ellipse the ellipse
     * @param position the position
     * @return PointAndDistance, the nearest point to the position and the distance from the ellipse
     */
    abstract PointAndDistance computeNearPoint(UnwritableEllipse ellipse, VectorIJK position);

  };

  /**
   * 
   * @return the default ellipse. This is a circle of radius one, with semi-major and semi-minor
   *         axes configured to be {@link VectorIJK.I} and {@link VectorIJK.J} respectively.
   */
  public static Ellipse create() {
    Ellipse ellipse = new Ellipse();
    return ellipse;
  }

  /**
   * Creates an ellipse from the center and generating vectors.
   * 
   * @param center the center vector
   * @param u a generating vector, may be {@link VectorIJK#ZERO}
   * @param v another generating vector, may be {@link VectorIJK#ZERO} or parallel/anti-parallel to
   *        u.
   * 
   * @return the ellipse from the center and generating vectors.
   */
  public static Ellipse createWithGeneratingVectors(UnwritableVectorIJK center,
      UnwritableVectorIJK u, UnwritableVectorIJK v) {
    Ellipse ellipse = new Ellipse();
    ellipse.setToGenerating(center, u, v);
    return ellipse;
  }

  /**
   * Creates an ellipse from the center, semiMajor, and semiMinor. Checks if the semiMajor and
   * semiMinor are orthogonal.
   * 
   * @param center the center vector
   * @param semiMajor major axis
   * @param semiMinor must be an orthogonal vector to semiMajor
   * @return
   */
  public static Ellipse create(UnwritableVectorIJK center, UnwritableVectorIJK semiMajor,
      UnwritableVectorIJK semiMinor) {
    Ellipse ellipse = new Ellipse();
    ellipse.setTo(center, semiMajor, semiMinor);
    return ellipse;
  }

  public static Ellipse create(UnwritableEllipse ellipse) {
    Ellipse newEllipse = new Ellipse();
    newEllipse.setTo(ellipse);
    return newEllipse;
  }

  /**
   * Creates the default ellipse. This is a circle of radius one, with semi-major and semi-minor
   * axes configured to be {@link VectorIJK.I} and {@link VectorIJK.J} respectively.
   */
  public Ellipse() {
    super();
  }

  @Override
  public Ellipse setTo(UnwritableEllipse ellipse) {
    super.setTo(ellipse);
    return this;
  }

  @Override
  public Ellipse setTo(UnwritableVectorIJK center, UnwritableVectorIJK semiMajor,
      UnwritableVectorIJK semiMinor) {
    super.setTo(center, semiMajor, semiMinor);
    return this;
  }

  @Override
  public Ellipse setToGenerating(UnwritableVectorIJK center, UnwritableVectorIJK u,
      UnwritableVectorIJK v) {
    super.setToGenerating(center, u, v);
    return this;
  }

  @Override
  public Ellipse rotate(UnwritableRotationMatrixIJK rotation) {
    super.rotate(rotation);
    return this;
  }

  @Override
  public Ellipse offset(UnwritableVectorIJK offset) {
    super.offset(offset);
    return this;
  }

  /**
   * Projects an ellipse onto a plane.
   * 
   * @param ellipse the ellipse to project
   * @param plane the plane onto which to project ellipse
   * @param buffer an ellipse buffer to receive the result of the projection; buffer may overwrite
   *        ellipse
   * 
   * @return a reference to buffer for convenience
   */
  public static Ellipse projectOnto(UnwritableEllipse ellipse, UnwritablePlane plane,
      Ellipse buffer) {

    /*
     * Allocate the buffers into which we will be inserting the center and generating vectors. This
     * allows ellipse to overwrite buffer, if they are the same.
     */
    VectorIJK u = new VectorIJK();
    VectorIJK v = new VectorIJK();
    VectorIJK w = new VectorIJK();

    plane.getNormal(w);

    /*
     * Project the ellipse semi-major and semi-minor axes onto the plane.
     */
    VectorIJK.planeProject(ellipse.smajor, w, u);
    VectorIJK.planeProject(ellipse.sminor, w, v);

    /*
     * Now project the center of ellipse onto plane, this will be the center of the projection.
     */
    plane.projectOnto(ellipse.center, w);

    /*
     * Configure the buffer from the new generating vectors and center.
     */
    return buffer.setToGenerating(w, u, v);

  }

  /**
   * Projects an ellipse onto a plane.
   * 
   * @param ellipse the ellipse to project
   * @param plane the plane onto which to project ellipse
   * 
   * @return a newly created ellipse containing the requested project
   */
  public static Ellipse projectOnto(UnwritableEllipse ellipse, UnwritablePlane plane) {
    return projectOnto(ellipse, plane, new Ellipse());
  }


}
