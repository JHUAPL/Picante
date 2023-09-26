package picante.surfaces;

import static com.google.common.base.Preconditions.checkNotNull;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

class OffsetSurface implements Surface {

  private final Surface delegate;
  private final UnwritableVectorIJK offset;

  OffsetSurface(Surface delegate, UnwritableVectorIJK offset) {
    super();
    this.delegate = checkNotNull(delegate);
    // defensive copy
    this.offset = new UnwritableVectorIJK(checkNotNull(offset));
  }

  @Override
  public VectorIJK computeOutwardNormal(UnwritableVectorIJK surfacePoint, VectorIJK buffer) {

    UnwritableVectorIJK offsetSurfacePoint = VectorIJK.add(surfacePoint, offset, new VectorIJK());

    return delegate.computeOutwardNormal(offsetSurfacePoint, buffer);
  }

  @Override
  public boolean intersects(UnwritableVectorIJK source, UnwritableVectorIJK ray) {

    UnwritableVectorIJK offsetSource = VectorIJK.add(source, offset, new VectorIJK());

    return delegate.intersects(offsetSource, ray);
  }

  @Override
  public VectorIJK compute(UnwritableVectorIJK source, UnwritableVectorIJK ray, VectorIJK buffer) {

    UnwritableVectorIJK offsetSource = VectorIJK.add(source, offset, new VectorIJK());

    return VectorIJK.subtract(delegate.compute(offsetSource, ray, buffer), offset, buffer);
  }

}
