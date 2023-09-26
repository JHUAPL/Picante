package picante.surfaces;

import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

public interface SurfaceIntersectionComputer {

  boolean intersects(UnwritableVectorIJK source, UnwritableVectorIJK ray);

  /**
   * 
   * @param source
   * @param ray
   * @param buffer
   * @return
   * 
   * @throws NoIntersectionException if ray emanating from source fails to intersect the surface
   */
  VectorIJK compute(UnwritableVectorIJK source, UnwritableVectorIJK ray, VectorIJK buffer);

}
