package picante.math.functions;

import picante.math.vectorspace.MatrixIJK;

/**
 * Simple interface for describing a single variable, differentiable matrix function.
 */
public interface DifferentiableMatrixIJKFunction extends MatrixIJKFunction {

  /**
   * Evaluates the derivative of the matrix function.
   * 
   * @param t the value of interest
   * 
   * @return the resultant differentiation
   */
  public default MatrixIJK differentiate(double t) {
    return differentiate(t, new MatrixIJK());
  }

  /**
   * Evaluates the derivative of the matrix function.
   * 
   * @param t the value of interest
   * @param buffer the buffer to receive the results
   * 
   * @return a reference to buffer for convenience
   */
  public MatrixIJK differentiate(double t, MatrixIJK buffer);

}
