package picante.math.functions;

import picante.math.vectorspace.VectorIJK;

/**
 * Simple interface for describing a single variable, differentiable vector function
 */
public interface DifferentiableVectorIJKFunction extends VectorIJKFunction {

  /**
   * Evaluates the derivative of the vector function.
   * 
   * @param t the value of interest
   * 
   * @return the resultant differentiation
   */
  public default VectorIJK differentiate(double t) {
    return differentiate(t, new VectorIJK());
  }

  /**
   * Evaluates the derivative of the vector function.
   * 
   * @param t the value of interest
   * @param buffer the buffer to receive the results
   * 
   * @return a reference to buffer for convenience
   */
  public VectorIJK differentiate(double t, VectorIJK buffer);

}
