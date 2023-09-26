package picante.surfaces;

import java.util.Optional;
import com.google.common.base.Preconditions;
import picante.math.PicanteMath;
import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

public class SurfaceUtilities {

  private SurfaceUtilities() {};

  /**
   * Compute the vector for a point on an ellipse from the observer
   * 
   * @param ellipse
   * @param theta
   * @return vector = center + cos(theta)*semiMajor + sin(theta)*semiMinor
   */
  public static UnwritableVectorIJK computeVectorOnEllipse(UnwritableEllipse ellipse,
      double theta) {
    UnwritableVectorIJK compositeAxis = VectorIJK.combine(1.0, ellipse.getCenter(), Math.cos(theta),
        ellipse.getSemiMajorAxis(), Math.sin(theta), ellipse.getSemiMinorAxis());
    return compositeAxis;
  }

  /**
   * Compute the vector for a point on an ellipse from the center of the ellipse
   * 
   * @param ellipse
   * @param theta
   * @return vector = cos(theta)*semiMajor + sin(theta)*semiMinor
   */
  public static UnwritableVectorIJK computeVectorOnEllipseCenter(UnwritableEllipse ellipse,
      double theta) {
    UnwritableVectorIJK compositeAxis = VectorIJK.combine(Math.cos(theta),
        ellipse.getSemiMajorAxis(), Math.sin(theta), ellipse.getSemiMinorAxis());
    return compositeAxis;
  }


  /**
   * Calculates the angle of the vector projected onto the ellipse plane.
   * 
   * @param vector
   * @param ellipse
   * @return angle defined by projected vector = cos(theta)*primary + sin(theta)*secondary
   */
  public static double computeAngleOnEllipse(UnwritableVectorIJK vector,
      UnwritableEllipse ellipse) {
    UnwritableVectorIJK vectorProjectOnPlane =
        VectorIJK.planeProject(vector, ellipse.getPlane().getNormal());
    Preconditions.checkArgument(vectorProjectOnPlane.getLength() > 0.001,
        "Vector must not be orthogonal to the ellipse plane.");

    /*
     * Ensure theta is properly oriented to the ellipse in a right hand system by flipping the angle
     * if facing away from the semiMinor axis.
     * 
     * vector = center + cos(theta)*semiMajor + sin(theta)*semiMinor
     */
    double angle = vectorProjectOnPlane.getSeparation(ellipse.getSemiMajorAxis());
    boolean negativeAngle = ellipse.getSemiMinorAxis().getDot(vectorProjectOnPlane) < 0;
    if (negativeAngle) {
      angle = -angle;
    }
    return angle;

  }

  /**
   * 
   * Computes the angle interval between the intersection of an ellipse with a limb plane. There are
   * two valid answers, which are complements of each other.
   * <p>
   * vector = center + cos(theta)*semiMajor + sin(theta)*semiMinor
   * <p>
   * 
   * @param ellipseWithAxis
   * @param plane
   * @return
   */
  public static Optional<UnwritableInterval> computeIntersectionAngles(UnwritableEllipse ellipse,
      Plane plane) {
    if (ellipse.intersects(plane)) {
      VectorIJK intersect1 = new VectorIJK();
      VectorIJK intersect2 = new VectorIJK();
      ellipse.intersect(plane, intersect1, intersect2);
      VectorIJK offsetIntersect1 = VectorIJK.subtract(intersect1, ellipse.getCenter());
      VectorIJK offsetIntersect2 = VectorIJK.subtract(intersect2, ellipse.getCenter());
      double theta1 = computeAngleOnEllipse(offsetIntersect1, ellipse);
      double theta2 = computeAngleOnEllipse(offsetIntersect2, ellipse);
      if (theta1 < theta2) {
        return Optional.of(new UnwritableInterval(theta1, theta2));
      } else {
        return Optional.of(new UnwritableInterval(theta2, theta1));
      }
    }
    return Optional.empty();
  }

  /**
   * 
   * Computes the angle interval between the intersection of an ellipse with a limb plane, and
   * adjusts the angle interval to be on the side closest to the origin of the limb.
   * 
   * for the ellipse:
   * 
   * angle defined by vector = center + cos(theta)*semiMajor + sin(theta)*semiMinor
   * 
   * @param ellipse the ellipse being intersected
   * @param intersector the ellipse whose plane will intersect the original ellipse
   * @return
   */
  public static Optional<UnwritableInterval> computeNearestIntersectionAngleInterval(
      UnwritableEllipse ellipse, UnwritableEllipse intersector) {
    return computeNearestIntersectionAngleInterval(ellipse, intersector.getPlane(),
        intersector.getCenter());
  }

  /**
   * 
   * Computes the angle interval between the intersection of an ellipse with plane, and adjusts the
   * angle interval to be on the side closest the obs2plane origin.
   * 
   * for the ellipse:
   * 
   * angle defined by vector = center + cos(theta)*semiMajor + sin(theta)*semiMinor
   * 
   * @param ellipse
   * @param plane
   * @param obs2plane
   * @return
   */
  public static Optional<UnwritableInterval> computeNearestIntersectionAngleInterval(
      UnwritableEllipse ellipse, Plane plane, UnwritableVectorIJK obs2plane) {
    Optional<UnwritableInterval> optionalRawIntersectionAngles =
        computeIntersectionAngles(ellipse, plane);
    if (optionalRawIntersectionAngles.isPresent()) {
      UnwritableInterval rawIntersectionAngles = optionalRawIntersectionAngles.get();
      /*
       * Get middle angle between intersection points, and compute the resulting vector on the
       * ellipse
       */
      double midTheta = rawIntersectionAngles.getMiddle();
      UnwritableVectorIJK ellipse2midPoint = computeVectorOnEllipseCenter(ellipse, midTheta);
      /*
       * If the resulting vector is closer to the observer, return the angles
       */
      if (obs2plane.getDot(ellipse2midPoint) < 0) {
        return Optional.of(rawIntersectionAngles);
      }
      /*
       * Otherwise, return the complement of the angle interval
       */
      else {
        return Optional.of(new UnwritableInterval(rawIntersectionAngles.getEnd(),
            rawIntersectionAngles.getBegin() + PicanteMath.PI * 2));
      }
    } else {
      return Optional.empty();
    }

  }

  /**
   * Creates a parallel on the specified latitude on the ellipsoid, offset by the
   * obsToBodyInBodyFrame vector.
   * 
   * @param bodyEllipsoid
   * @param obsToBodyInBodyFrame
   * @param latitude
   * @return
   */
  public static Ellipse createParallel(Ellipsoid bodyEllipsoid,
      UnwritableVectorIJK obsToBodyInBodyFrame, Double latitude) {
    UnwritableVectorIJK xVec = VectorIJK.I.createScaled(bodyEllipsoid.getA());
    UnwritableVectorIJK yVec = VectorIJK.J.createScaled(bodyEllipsoid.getB());
    UnwritableVectorIJK spinVec = VectorIJK.K.createScaled(bodyEllipsoid.getC());
    UnwritableVectorIJK xzParallelVec =
        VectorIJK.combine(Math.cos(latitude), xVec, Math.sin(latitude), spinVec);
    UnwritableVectorIJK yzParallelVec =
        VectorIJK.combine(Math.cos(latitude), yVec, Math.sin(latitude), spinVec);
    UnwritableVectorIJK center =
        VectorIJK.add(spinVec.createScaled(xzParallelVec.getDot(spinVec) / spinVec.getDot(spinVec)),
            obsToBodyInBodyFrame);
    UnwritableVectorIJK primaryAxis =
        xVec.createScaled(xzParallelVec.getDot(xVec) / xVec.getDot(xVec));
    UnwritableVectorIJK secondaryAxis =
        yVec.createScaled(yzParallelVec.getDot(yVec) / yVec.getDot(yVec));
    return Ellipse.create(center, primaryAxis, secondaryAxis);
  }

  /**
   * Creates meridian on the specified longitude on the ellipsoid, offset by the
   * obsToBodyInBodyFrame vector.
   * 
   * @param bodyEllipsoid
   * @param obsToBodyInBodyFrame
   * @param longitude
   * @return
   */
  public static Ellipse createMeridian(Ellipsoid bodyEllipsoid,
      UnwritableVectorIJK obsToBodyInBodyFrame, Double longitude) {
    UnwritableVectorIJK xVec = VectorIJK.I.createScaled(bodyEllipsoid.getA());
    UnwritableVectorIJK yVec = VectorIJK.J.createScaled(bodyEllipsoid.getB());
    UnwritableVectorIJK center = obsToBodyInBodyFrame;
    /*
     * Define a vector on the equator (appropriately sized with the radii)
     */
    UnwritableVectorIJK primaryAxis =
        VectorIJK.combine(Math.cos(longitude), xVec, Math.sin(longitude), yVec);
    /*
     * Define a north polar vector (appropriately sized with the radii)
     */
    UnwritableVectorIJK secondaryAxis = VectorIJK.K.createScaled(bodyEllipsoid.getC());
    return Ellipse.create(center, primaryAxis, secondaryAxis);
  }

}
