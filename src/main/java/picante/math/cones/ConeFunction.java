package picante.math.cones;

/**
 * A univariate function that returns a cone.
 * <p>
 * The parameter of the function would usually be time, to capture the evolution of a cone in time.
 * </p>
 * 
 * @author C.M. O'Shea
 * @author R.T. Poffenbarger
 *
 */
public interface ConeFunction {

  /**
   * Returns a cone as evaluated at the specified parameter t.
   * 
   * @param t the parameter
   * 
   * @return the cone
   */
  Cone evaluate(double t);

}
