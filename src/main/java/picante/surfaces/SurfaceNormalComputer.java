package picante.surfaces;

import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * Defines the outward directed normal vector for a surface.
 */
public interface SurfaceNormalComputer {

  /**
   * Computes the outward directed normal of a surface at the specified point.
   * <p>
   * Note: while it may be useful to normalize the resultant vector, there is no specific
   * requirement to do so.
   * </p>
   * 
   * @param surfacePoint the point at which to compute the outward
   * 
   * @return the normal vector
   */
  public default VectorIJK computeOutwardNormal(UnwritableVectorIJK surfacePoint) {
    return computeOutwardNormal(surfacePoint, new VectorIJK());
  }

  /**
   * Computes the outward directed normal of a surface at the specified point.
   * <p>
   * Note: while it may be useful to normalize the resultant vector, there is no specific
   * requirement to do so.
   * </p>
   * 
   * @param surfacePoint the point at which to compute the outward
   * @param buffer a buffer to capture the normal vector
   * 
   * @return a reference to buffer for convenience
   */
  public VectorIJK computeOutwardNormal(UnwritableVectorIJK surfacePoint, VectorIJK buffer);

}
