package picante.surfaces;

import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;

public class Surfaces {

  private static final UnwritableRotationMatrixIJK ROTATE_Z_TO_X =
      new UnwritableRotationMatrixIJK(0.0, 0.0, 1.0, 0.0, 1.0, 0.0, -1.0, 0.0, 0.0);

  private static final UnwritableRotationMatrixIJK ROTATE_Z_TO_Y =
      new UnwritableRotationMatrixIJK(1.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 1.0, 0.0);

  private Surfaces() {}

  public static Ellipsoid createEllipsoidalSurface(double a, double b, double c) {
    if (a == b && a == c) {
      return new Sphere(a);
    }

    return new Ellipsoid(a, b, c);
  }

  public static Sphere createSphere(double radius) {
    return new Sphere(radius);
  }

  public static Surface offset(final Surface surface, final UnwritableVectorIJK offset) {
    return new OffsetSurface(surface, offset);
  }

  public static Surface createCylinderAlongX(double radius) {
    return new RotatedSurface(new Cylinder(radius), ROTATE_Z_TO_X);
  }

  public static Surface createCylinderAlongY(double radius) {
    return new RotatedSurface(new Cylinder(radius), ROTATE_Z_TO_Y);
  }

  public static Surface createCylinderAlongZ(double radius) {
    return new Cylinder(radius);
  }

}
