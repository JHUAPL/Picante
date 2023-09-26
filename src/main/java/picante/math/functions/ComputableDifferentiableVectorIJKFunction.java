package picante.math.functions;

import picante.math.functions.Computable.ComputableResult;
import picante.math.vectorspace.VectorIJK;

/**
 * A {@link DifferentiableVectorIJKFunction} that is also {@link Computable}. The purpose of this
 * mix is to facilitate performance improvements in cases where 1) the function and its derivative
 * are often both needed by calling code, and 2) it is more efficient to compute the function and
 * its derivative in a single set of operations. To take advantage of such improvements, any method
 * that accepts DifferentiableVectorIJKFunction may safely be modified to check whether an object
 * passed also implements this interface and include code optimized for this case.
 * <p>
 * This interface is generic, templated on the specific type of the {@link ComputableResult}
 * returned in order to provide some type safety.
 * 
 * @author James Peachey
 *
 */
public interface ComputableDifferentiableVectorIJKFunction<R extends ComputableResult>
    extends Computable, DifferentiableVectorIJKFunction {

  /**
   * {@inheritDoc}
   * <p>
   * Overridden to guarantee results from implememtations of this computation may be used as input
   * to methods defined on this interface.
   */
  @Override
  R compute(double t);

  /**
   * Extract and return the value of the vector function from a {@link ComputableResult} that was
   * previously returned by the compute(double t) method. If result = compute(t1) for a particular
   * t1, it is required that evaluate(result, buffer) return the exact same value as evaluate(t1,
   * buffer).
   * 
   * @param prior the result returned by a prior call to compute(t1)
   * @param buffer the buffer to capture the resultant evaluation
   * @return a reference to the buffer for convenience
   */
  VectorIJK evaluate(R prior, VectorIJK buffer);

  /**
   * Extract and return the value of the derivative of the vector function from a
   * {@link ComputableResult} that was previously returned by the compute(double t) method. If
   * result = differentiate(t1) for a particular t1, it is required that differentiate(result,
   * buffer) return the exact same value as differentiate(t1, buffer).
   * 
   * @param prior the result returned by a prior call to compute(t1)
   * @param buffer the buffer to receive the results
   * 
   * @return a reference to buffer for convenience
   */
  VectorIJK differentiate(R prior, VectorIJK buffer);

}
