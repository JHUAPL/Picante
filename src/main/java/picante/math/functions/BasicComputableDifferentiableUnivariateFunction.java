package picante.math.functions;

import picante.math.functions.Computable.ComputableResult;

/**
 * Basic abstract implementation of {@link ComputableDifferentiableUniveriateFunction} that ensures
 * its contract is met.
 * 
 * @author James Peachey
 *
 */
public abstract class BasicComputableDifferentiableUnivariateFunction<R extends ComputableResult>
    implements ComputableDifferentiableUnivariateFunction<R> {

  /**
   * {@inheritDoc}
   * <p>
   * Code that calls this method may not perform as quickly as a custom implementation of this
   * method would, since the compute method also computes the derivative. Subclasses may optionally
   * override this behavior to improve performance in such cases.
   */
  @Override
  public double evaluate(double t) {
    return evaluate(compute(t));
  }

  /**
   * {@inheritDoc}
   * <p>
   * Code that calls this method is likely to perform as quickly as a custom implementation of this
   * method would, since the compute method also computes the derivative. Subclasses may optionally
   * override this behavior to improve performance if this proves not to be the case.
   */
  @Override
  public double differentiate(double t) {
    return differentiate(compute(t));
  }

}
