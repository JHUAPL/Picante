package picante.math.functions;

import picante.math.vectorspace.VectorIJK;

/**
 * Simple interface for describing a VectorIJK function of a single variable.
 */
public interface VectorIJKFunction {

  /**
   * Evaluates the vector function at the specified value
   * 
   * @param t the value at which to evaluate the function
   * 
   * @return the resultant evaluation
   */
  public default VectorIJK evaluate(double t) {
    return evaluate(t, new VectorIJK());
  }

  /**
   * Evaluates the vector function at the specified value into the supplied buffer
   * 
   * @param t the value at which to evaluate the function
   * @param buffer the buffer to capture the resultant evaluation
   * 
   * @return a reference to buffer for convenience
   */
  public VectorIJK evaluate(double t, VectorIJK buffer);

}
