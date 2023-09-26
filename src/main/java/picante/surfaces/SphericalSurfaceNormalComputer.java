package picante.surfaces;

import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * Trivial implementation of the {@link SurfaceNormalComputer} interface for a spherical target
 * body.
 */
class SphericalSurfaceNormalComputer implements SurfaceNormalComputer {

  @Override
  public VectorIJK computeOutwardNormal(UnwritableVectorIJK surfacePoint, VectorIJK buffer) {
    /*
     * There's nothing to do, the surface normal is the surface point vector as this is simply a
     * sphere.
     */
    return buffer.setTo(surfacePoint);
  }

}
