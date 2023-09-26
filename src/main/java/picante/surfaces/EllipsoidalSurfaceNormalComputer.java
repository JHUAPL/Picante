package picante.surfaces;

import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

class EllipsoidalSurfaceNormalComputer implements SurfaceNormalComputer {

  private final double a1;
  private final double b1;
  private final double c1;

  EllipsoidalSurfaceNormalComputer(double a, double b, double c, double minRadius) {
    this.a1 = minRadius / a;
    this.b1 = minRadius / b;
    this.c1 = minRadius / c;
  }

  @Override
  public VectorIJK computeOutwardNormal(UnwritableVectorIJK surfacePoint, VectorIJK buffer) {
    /*
     * Compute (surfacePoint.getI() /a/a, surfacePoint.getJ()/b/b, surfacePoint.getK()/c/c), but
     * this could be numerically unwise due to overflow. Use minRadius as a scaling.
     */
    buffer.setTo(surfacePoint.getI() * (a1 * a1), surfacePoint.getJ() * (b1 * b1),
        surfacePoint.getK() * (c1 * c1));

    return buffer;

  }

}
