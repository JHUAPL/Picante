package picante.surfaces;

import static com.google.common.base.Preconditions.checkArgument;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

public class Sphere extends Ellipsoid {

  private final SphericalSurfaceNormalComputer normalComputer;

  Sphere(double radius) {
    super(radius, radius, radius);
    checkArgument(radius > 0, "A sphere's radius must be strictly greater than zero.");
    this.normalComputer = new SphericalSurfaceNormalComputer();
  }

  public double getRadius() {
    return getA();
  }

  @Override
  public VectorIJK computeOutwardNormal(UnwritableVectorIJK surfacePoint, VectorIJK buffer) {
    return normalComputer.computeOutwardNormal(surfacePoint, buffer);
  }

}
