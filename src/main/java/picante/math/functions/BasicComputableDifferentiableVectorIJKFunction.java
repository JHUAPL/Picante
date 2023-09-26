package picante.math.functions;

import picante.math.functions.Computable.ComputableResult;
import picante.math.vectorspace.VectorIJK;

/**
 * Basic abstract implementation of {@link ComputableDifferentiableVectorIJKFunction} that ensures
 * its contract is met.
 * 
 * @author James Peachey
 *
 */
public abstract class BasicComputableDifferentiableVectorIJKFunction<R extends ComputableResult>
    implements ComputableDifferentiableVectorIJKFunction<R> {

  /**
   * {@inheritDoc}
   * <p>
   * Code that calls this method may not perform as quickly as a custom implementation of this
   * method would, since the compute method also computes the derivative. Subclasses may optionally
   * override this behavior to improve performance in such cases.
   */
  @Override
  public final VectorIJK evaluate(double t, VectorIJK buffer) {
    return evaluate(compute(t), buffer);
  }

  /**
   * {@inheritDoc}
   * <p>
   * Code that calls this method is likely to perform as quickly as a custom implementation of this
   * method would, since the compute method also computes the derivative. Subclasses may optionally
   * override this behavior to improve performance if this proves not to be the case.
   */
  @Override
  public final VectorIJK differentiate(double t, VectorIJK buffer) {
    return differentiate(compute(t), buffer);
  }

}
