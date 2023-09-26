package picante.math.functions;

import picante.math.functions.Computable.ComputableResult;

/**
 * A {@link DifferentiableUnivariateFunction} that is also {@link Computable}. The purpose of this
 * mix is to facilitate performance improvements in cases where 1) the function and its derivative
 * are often both needed by calling code, and 2) it is more efficient to compute the function and
 * its derivative in a single set of operations. To take advantage of such improvements, any method
 * that accepts DifferentiableUnivariateFunction may safely be modified to check whether an object
 * passed also implements this interface and include code optimized for this case.
 * <p>
 * This interface is generic, templated on the specific type of the {@link ComputableResult}
 * returned in order to provide some type safety.
 * 
 * @author James Peachey
 *
 */
public interface ComputableDifferentiableUnivariateFunction<R extends ComputableResult>
    extends Computable, DifferentiableUnivariateFunction {

  /**
   * {@inheritDoc}
   * <p>
   * Overridden to guarantee results from implememtations of this computation may be used as input
   * to methods defined on this interface.
   */
  @Override
  R compute(double t);

  /**
   * Extract and return the value of the function from an object of {@link ComputableResult} that
   * was previously returned by the compute(double t) method. If result = compute(t1) for a
   * particular t1, it is required that evaluate(result) return the exact same value as
   * evaluate(t1).
   * 
   * @param prior the result returned by a prior call to compute(t1)
   * @return the value of the function at the same value t1
   */
  double evaluate(R prior);

  /**
   * Extract and return the value of the derivative of the function from a {@link ComputableResult}
   * that was previously returned by the compute(double t) method. If result = compute(t1) for a
   * particular t1, it is required that differentiate(result) return the exact same value as
   * differentiate(t1).
   * 
   * @param prior the result returned by a previous call to compute(t1)
   * @return the value of the derivative of the function at the same value t1
   */
  double differentiate(R prior);

}
