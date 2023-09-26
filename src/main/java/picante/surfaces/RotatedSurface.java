package picante.surfaces;

import static com.google.common.base.Preconditions.checkNotNull;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

class RotatedSurface implements Surface {

  private final Surface delegate;
  private final UnwritableRotationMatrixIJK rotationMatrix;

  public RotatedSurface(Surface delegate, UnwritableRotationMatrixIJK rotationMatrix) {
    super();
    this.delegate = checkNotNull(delegate);
    this.rotationMatrix = new UnwritableRotationMatrixIJK(checkNotNull(rotationMatrix));
  }

  @Override
  public VectorIJK computeOutwardNormal(UnwritableVectorIJK surfacePoint, VectorIJK buffer) {
    return rotationMatrix
        .mtxv(delegate.computeOutwardNormal(rotationMatrix.mxv(surfacePoint), buffer), buffer);
  }

  @Override
  public boolean intersects(UnwritableVectorIJK source, UnwritableVectorIJK ray) {
    return delegate.intersects(rotationMatrix.mxv(source), rotationMatrix.mxv(ray));
  }

  @Override
  public VectorIJK compute(UnwritableVectorIJK source, UnwritableVectorIJK ray, VectorIJK buffer) {
    return rotationMatrix.mtxv(
        delegate.compute(rotationMatrix.mxv(source), rotationMatrix.mxv(ray), buffer), buffer);
  }

}
