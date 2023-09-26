package picante.surfaces;

import static com.google.common.base.Preconditions.checkArgument;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

class EllipsoidalLimbComputer {

  private final double a;
  private final double b;
  private final double c;

  private final EllipsoidalPlaneIntersectionComputer planeComputer;

  private final double scale;
  private final double scaledASq;
  private final double scaledBSq;
  private final double scaledCSq;

  EllipsoidalLimbComputer(double a, double b, double c) {
    this.a = a;
    this.b = b;
    this.c = c;

    scale = Math.max(Math.abs(a), Math.max(Math.abs(b), Math.abs(c)));

    double scaledA = this.a / scale;
    double scaledB = this.b / scale;
    double scaledC = this.c / scale;

    this.planeComputer = new EllipsoidalPlaneIntersectionComputer(scaledA, scaledB, scaledC);

    this.scaledASq = scaledA * scaledA;
    this.scaledBSq = scaledB * scaledB;
    this.scaledCSq = scaledC * scaledC;
  }

  private double level(VectorIJK vector) {
    return vector.getI() * vector.getI() / scaledASq + vector.getJ() * vector.getJ() / scaledBSq
        + vector.getK() * vector.getK() / scaledCSq;
  }

  Ellipse computeLimb(UnwritableVectorIJK viewPoint, Ellipse buffer) {

    /*
     * Scale the viewpoint.
     */
    VectorIJK v = new VectorIJK(viewPoint);
    v.scale(1.0 / scale);

    /*
     * Determine if the viewing point lies outside the ellipsoid. This amounts to comparing the
     * level of the viewpoint is greater than or equal to 1.0.
     */
    checkArgument(level(v) >= 1.0, "Viewing point is inside the body.");

    /*
     * Find a normal vector for the limb plane.
     */
    v.setTo(v.getI() / scaledASq, v.getJ() / scaledBSq, v.getK() / scaledCSq);
    Plane p = new Plane(v, 1.0);

    /*
     * Find the limb by intersecting the limb plane with the ellipsoid. Note: this will throw an
     * illegal argument exception of the geometry is sufficiently extreme. TODO: Should this
     * exception be wrapped?
     */
    planeComputer.intersect(p, buffer);

    /*
     * Undo the scaling to each element of the ellipse.
     */
    buffer.scale(scale);

    return buffer;

  }

}
