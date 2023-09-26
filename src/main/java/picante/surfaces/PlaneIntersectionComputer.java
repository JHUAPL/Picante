package picante.surfaces;

import static com.google.common.base.Preconditions.checkArgument;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

public class PlaneIntersectionComputer implements SurfaceIntersectionComputer {

  /**
   * The plane constant. In the canonical form this is always non-negative and captures the minimum
   * distance to the plane from the origin.
   */
  private final double constant;

  /**
   * The unit normal that points from the origin to the point closest in the plane.
   */
  private final VectorIJK normal;

  private static final double MARGIN = 3.0;


  public PlaneIntersectionComputer(double constant, VectorIJK normal) {
    super();
    this.constant = constant;
    this.normal = normal;
  }

  @Override
  public boolean intersects(UnwritableVectorIJK source, UnwritableVectorIJK ray) {
    double tooBig = Double.MAX_VALUE / MARGIN;
    checkInput(source, ray, tooBig);

    // direction of ray
    UnwritableVectorIJK rayDir = ray.createUnitized();

    /*
     * Now scale the input vertex and plane to improve numerical behavior
     */
    VectorIJK scaledVertex = new VectorIJK();
    double scaledConstant;
    double mScale = Math.max(constant, source.getLength());
    if (mScale != 0) {
      scaledVertex.setTo(source.createScaled(1. / mScale));
      scaledConstant = constant / mScale;
    } else {
      scaledVertex.setTo(source);
      scaledConstant = constant;
    }

    if (mScale > 1.) {
      tooBig = tooBig / mScale;
    }

    /*
     * Find the projection of the ray's vertex along the plane's normal direction
     */
    double projeVertex = scaledVertex.getDot(normal);


    /*
     * If this projection is the plane constant, the ray's vertex lies in the plane. We have one
     * intersection or an infinite number of intersections. It all depends on whether the ray
     * actually lies in the plane. Consider infinite intersections to be NO intersection.
     *
     * The absolute value of rayToPlaneDistance is the distance of the ray's vertex from the plane.
     */
    double rayToPlaneDistance = scaledConstant - projeVertex;

    if (rayToPlaneDistance == 0.) {
      // vertex lies in plane
      if (normal.getDot(rayDir) == 0.) {
        // the ray is in the plane. return false
        return false;
      }
      return true;
    }

    /*
     * The ray's vertex is not in the plane. The ray could still be parallel to or point away from
     * the plane. Find the projection of the direction vector along the plane's normal vector.
     */
    double projDirection = rayDir.getDot(normal);

    /*
     * We're done if the ray doesn't point toward the plane. rayToPlaneDistance has already been
     * found to be non-zero at this point; projDirection is zero if the ray and plane are parallel.
     */
    if (!sameSign(projDirection, rayToPlaneDistance)) {
      // the ray is parallel to or points away from the plane
      return false;
    }

    if (Math.abs(rayToPlaneDistance) >= Math.abs(projDirection) * tooBig) {
      // If the hypotenuse is too long, we say that no intersection exists
      return false;
    }

    return true;
  }

  @Override
  public VectorIJK compute(UnwritableVectorIJK source, UnwritableVectorIJK ray, VectorIJK buffer) {
    double tooBig = Double.MAX_VALUE / MARGIN;
    checkInput(source, ray, tooBig);

    // direction of ray
    UnwritableVectorIJK rayDir = ray.createUnitized();

    /*
     * Now scale the input vertex and plane to improve numerical behavior
     */
    VectorIJK scaledVertex = new VectorIJK();
    double scaledConstant;
    double mScale = Math.max(constant, source.getLength());
    if (mScale != 0) {
      scaledVertex.setTo(source.createScaled(1. / mScale));
      scaledConstant = constant / mScale;
    } else {
      scaledVertex.setTo(source);
      scaledConstant = constant;
    }

    if (mScale > 1.) {
      tooBig = tooBig / mScale;
    }

    /*
     * Find the projection of the ray's vertex along the plane's normal direction
     */
    double projVertex = scaledVertex.getDot(normal);


    /*
     * If this projection is the plane constant, the ray's vertex lies in the plane. We have one
     * intersection or an infinite number of intersections. It all depends on whether the ray
     * actually lies in the plane.
     *
     * The absolute value of PRJDIF is the distance of the ray's vertex from the plane.
     */
    double rayToPlaneDistance = scaledConstant - projVertex;

    if (rayToPlaneDistance == 0.) {
      // vertex lies in plane, so xpt is the original, uncaled vertex
      buffer.setTo(source);

      if (normal.getDot(rayDir) == 0.) {
        // the ray is in the plane.
        throw new NoIntersectionException(
            "The input ray lies in the plane. There are an infinite number of intersections");
      } else {
        return buffer;
      }
    }

    /*
     * The ray's vertex is not in the plane. The ray could still be parallel to or point away from
     * the plane. In order to find an intersection, the following must be true:
     * 
     * -- The ray must point toward the plane; this happens when rayToPlaneDistance has the same
     * sign as < rayDir, theNormal >.
     * 
     * -- The vector difference buffer - scaledVector must not overflow.
     */

    /*
     * Find the projection of the direction vector along the plane's normal vector.
     */
    double projDirection = rayDir.getDot(normal);

    /*
     * We're done if the ray doesn't point toward the plane. rayToPlaneDistance has already been
     * found to be non-zero at this point; projDirection is zero if the ray and plane are parallel.
     */
    if (!sameSign(projDirection, rayToPlaneDistance)) {
      // the ray is parallel to or points away from the plane
      throw new NoIntersectionException("No intersection exists");
    }

    if (Math.abs(rayToPlaneDistance) >= Math.abs(projDirection) * tooBig) {
      // If the hypotenuse is too long, we say that no intersection exists
      throw new NoIntersectionException("No intersection exists");
    }

    /*
     * now it is safe to compute the intersection point. Scale rayDir and add the result to
     * scaledVector. The addition is safe because both addends have magnitude no larger than tooBig.
     * The resultant vector is the intersection point.
     */
    double scale = Math.abs(rayToPlaneDistance) / Math.abs(projDirection);
    VectorIJK.combine(1.0, scaledVertex, scale, rayDir, buffer);
    buffer.scale(mScale);
    return buffer;

  }

  /**
   * utility to check the input ray and source vectors for length and distance from origin
   * 
   * @param source
   * @param ray
   * @param tooBig
   */
  private void checkInput(UnwritableVectorIJK source, UnwritableVectorIJK ray, double tooBig) {
    checkArgument(!ray.equals(VectorIJK.ZERO), "Query ray can not be of zero length.");

    /*
     * Check the distance of the ray's vertex from the origin.
     */
    if (source.getLength() >= tooBig) {
      throw new RuntimeException("Ray's vertex is too far from the origin");
    }

    /*
     * Check the distance of the plane from the origin
     */
    if (constant >= tooBig) {
      throw new RuntimeException("Plane is too far from the origin");
    }
  }

  private static boolean sameSign(double x, double y) {
    return (x > 0 && y > 0) || (x < 0 && y < 0);
  }

}
