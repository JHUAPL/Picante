package picante.math.functions;

/**
 * Simple interface describing a single variable, differentiable function.
 */
public interface DifferentiableUnivariateFunction extends UnivariateFunction {

  /**
   * Evaluates the derivative of the univariate function.
   * 
   * @param t the value of interest
   * 
   * @return the derivative of the function evaluated at t
   */
  public double differentiate(double t);

}
