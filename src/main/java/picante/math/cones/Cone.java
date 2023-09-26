package picante.math.cones;

import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * Interface parameterizing a cone in three dimensions.
 * <p>
 * In our current plan, these cones should either be convex or the complement of a convex cone.
 * Algorithms for manipulating implementations of this interface may implicitly assume that is the
 * case.
 * </p>
 * 
 * @author C.M. O'Shea
 * @author R.T. Poffenbarger
 *
 */
public interface Cone {

  /**
   * Retrieves the vector from the origin of the coordinate system to the cone's vertex.
   * 
   * @return a vector, may be {@link VectorIJK#ZERO}
   */
  UnwritableVectorIJK getVertex();

  /**
   * Retrieves a vector along the edge of the cone.
   * 
   * @param parameter the value of the parameter specifying which edge vector to select.
   * 
   * @return a vector, of non-zero length, that points from the vertex along the edge of the cone at
   *         parameter p
   */
  UnwritableVectorIJK getEdge(double parameter);

  /**
   * Retrieves the domain of the parameter p.
   * 
   * @return an interval, where {@link UnwritableInterval#getBegin()} and
   *         {@link UnwritableInterval#getEnd()} map to the same edge vector along the cone.
   */
  UnwritableInterval getParameterDomain();

  /**
   * Retrieves a vector in the interior of the cone.
   * 
   * @return the vector
   */
  UnwritableVectorIJK getInteriorPoint();
}
