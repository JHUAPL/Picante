package picante.math.functions;

import picante.exceptions.FunctionEvaluationException;
import picante.math.vectorspace.MatrixIJK;

/**
 * Simple interface for describing a MatrixIJK function of a single variable.
 * 
 */
public interface MatrixIJKFunction {

  /**
   * Evaluates the matrix function at the specified value
   * 
   * @param t the value at which to evaluate the function
   * 
   * @return the resultant evaluation
   */
  public default MatrixIJK evaluate(double t) {
    return evaluate(t, new MatrixIJK());
  }

  /**
   * Evaluates the matrix function at the specified value into the supplied buffer
   * 
   * @param t the value at which to evaluate the function
   * 
   * @param buffer the buffer to capture the resultant evaluation
   * 
   * @return a reference to buffer for convenience
   * 
   * @throws FunctionEvaluationException if the function cannot perform the evaluation
   */
  public MatrixIJK evaluate(double t, MatrixIJK buffer);

}
