package picante.math.functions;

import picante.exceptions.FunctionEvaluationException;

/**
 * Simple interface describing a function of a single variable.
 */
@FunctionalInterface
public interface UnivariateFunction {

  /**
   * Evaluates the function at the specified value.
   * 
   * @param t the value of interest
   * 
   * @return the value of the function at t
   * 
   * @throws FunctionEvaluationException if the function cannot perform the evaluation
   */
  public double evaluate(double t);

}
