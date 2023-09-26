package picante.surfaces;

import static com.google.common.base.Preconditions.checkArgument;
import picante.math.vectorspace.VectorIJK;

class EllipsoidalPlaneIntersectionComputer {

  private final double a;
  private final double b;
  private final double c;

  private final double maxRadius;

  EllipsoidalPlaneIntersectionComputer(double a, double b, double c) {
    this.a = a;
    this.b = b;
    this.c = c;
    this.maxRadius = Math.max(Math.abs(a), Math.max(Math.abs(b), Math.abs(c)));
  }

  private void distort(VectorIJK v) {
    v.setTo(v.getI() / a, v.getJ() / b, v.getK() / c);
  }

  private void distortInverse(VectorIJK v) {
    v.setTo(v.getI() * a, v.getJ() * b, v.getK() * c);
  }

  /**
   * Does a plane whose origin is coincident to the ellipsoid intersect it?
   * 
   * @param plane the plane
   * 
   * @return true if plane intersects this ellipsoid, false otherwise
   */
  boolean intersects(UnwritablePlane plane) {

    /*
     * Start out with the simple check, is the plane constant greater than the maximum radius? If
     * so, then clearly there is no intersection.
     */
    if (plane.getConstant() > maxRadius) {
      return false;
    }

    /*
     * Distort the input plane into the space where the ellipsoid is a unit sphere.
     */
    VectorIJK u = new VectorIJK();
    VectorIJK v = new VectorIJK();
    VectorIJK p = new VectorIJK();

    plane.getSpanningVectors(u, v);
    plane.getPoint(p);

    distort(u);
    distort(v);
    distort(p);

    Plane distortedPlane = new Plane(p, u, v);

    distortedPlane.getPoint(p);

    /*
     * The plane intersects the ellipse only if the point lies inside or on the unit sphere in the
     * distorted space.
     */
    return p.getLength() <= 1.0;
  }

  /**
   * Determine the ellipse of intersection of a plane with this ellipsoid.
   * 
   * @param plane
   * @param buffer
   * @return
   * 
   * @throws IllegalArgumentException if the plane does not intersect the ellipse.
   */
  Ellipse intersect(UnwritablePlane plane, Ellipse buffer) {

    checkArgument(plane.getConstant() <= maxRadius, "Plane does not intersect ellipse.");

    /*
     * Distort the input plane into the space where the ellipsoid is a unit sphere.
     */
    VectorIJK u = new VectorIJK();
    VectorIJK v = new VectorIJK();
    VectorIJK p = new VectorIJK();

    plane.getSpanningVectors(u, v);
    plane.getPoint(p);

    distort(u);
    distort(v);
    distort(p);

    Plane distortedPlane = new Plane(p, u, v);

    /*
     * The point and spanning vector retrieval methods on plane always return a point that is
     * closest to the origin in the input plane. This point is the center of the intersection
     * circle. The spanning
     */
    distortedPlane.getPoint(p);
    distortedPlane.getSpanningVectors(u, v);

    double dist = p.getLength();
    checkArgument(dist <= 1.0, "Plane does not intersect ellipse.");

    double radius = Math.sqrt(clampToUnitLength(1.0 - dist * dist));

    u.scale(radius);
    v.scale(radius);

    distortInverse(p);
    distortInverse(u);
    distortInverse(v);

    buffer.setToGenerating(p, u, v);

    return buffer;
  }

  private double clampToUnitLength(double value) {
    return Math.max(0.0, Math.min(value, 1.0));
  }

}
