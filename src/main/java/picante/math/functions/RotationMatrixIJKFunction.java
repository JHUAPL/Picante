package picante.math.functions;

import picante.exceptions.FunctionEvaluationException;
import picante.math.vectorspace.RotationMatrixIJK;

/**
 * Simple interface for describing a RotationMatrixIJK function of a single variable.
 */
public interface RotationMatrixIJKFunction {

  /**
   * Evaluates the matrix function at the specified value
   * 
   * @param t the value at which to evaluate the function
   * 
   * @return the resultant evaluation
   */
  public default RotationMatrixIJK evaluate(double t) {
    return evaluate(t, new RotationMatrixIJK());
  }

  /**
   * Evaluates the matrix function at the specified value into the supplied buffer
   * 
   * @param t the value at which to evaluate the function
   * @param buffer the buffer to capture the resultant evaluation
   * 
   * @return a reference to buffer for convenience
   * 
   * @throws FunctionEvaluationException if the function cannot perform the evaluation
   */
  public RotationMatrixIJK evaluate(double t, RotationMatrixIJK buffer);

}
