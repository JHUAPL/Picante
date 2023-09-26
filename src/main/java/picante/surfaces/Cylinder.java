package picante.surfaces;

import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJ;
import picante.math.vectorspace.VectorIJK;

class Cylinder implements Surface {

  private final Circle circle;

  Cylinder(double radius) {
    super();
    this.circle = new Circle(radius);
  }

  @Override
  public VectorIJK computeOutwardNormal(UnwritableVectorIJK surfacePoint, VectorIJK buffer) {
    return buffer.setTo(2 * surfacePoint.getI(), 2 * surfacePoint.getJ(), 0.0);
  }

  @Override
  public boolean intersects(UnwritableVectorIJK source, UnwritableVectorIJK ray) {
    return circle.computeIntersectionsRay(new VectorIJ(source.getI(), source.getJ()),
        new VectorIJ(ray.getI(), ray.getJ())).size() != 0;
  }

  @Override
  public VectorIJK compute(UnwritableVectorIJK source, UnwritableVectorIJK ray, VectorIJK buffer) {

    VectorIJ sourceij = new VectorIJ(source.getI(), source.getJ());
    VectorIJ rayij = new VectorIJ(ray.getI(), ray.getJ());

    if (sourceij.equals(rayij)) {
      throw new NoIntersectionException();
    }

    VectorIJ bufferij = circle.computeFirstIntersectionRay(sourceij, rayij, new VectorIJ());

    double m = ray.getK() / rayij.getLength();

    double zIntercept = m * (VectorIJ.subtract(bufferij, sourceij).getLength()) + source.getK();

    return buffer.setTo(bufferij.getI(), bufferij.getJ(), zIntercept);

  }
}
