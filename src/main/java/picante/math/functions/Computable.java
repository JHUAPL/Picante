package picante.math.functions;

/**
 * Interface representing some computation performed as a function of a single parameter t. The
 * result of the computation is in turn represented by a nested marker interface
 * {@link ComputableResult}. Implementations of Computable and ComputableResult in general need to
 * be developed in tandem. Abstracting the result returned allows computation for multiple related
 * values to be performed in a single call to the compute method.
 * 
 * @author James Peachey
 *
 */
public interface Computable {

  /**
   * Marker interface representing the result of a computation for a particular value of the
   * parameter t.
   *
   */
  public interface ComputableResult {

  }

  /**
   * Compute the result produced by this Computable for the provided input parameter, returned as an
   * instance of the ComputableResult interface.
   * 
   * @param t the input parameter value at which to compute the result
   * @return the result of the computation
   */
  ComputableResult compute(double t);
}
