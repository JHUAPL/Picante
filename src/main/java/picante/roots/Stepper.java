package picante.roots;

/**
 * Interface that provides the next step to take from the current value in the search.
 * <p>
 * For commonly used implementations, see {@link Steppers}.
 * </p>
 * 
 */
public interface Stepper {

  /**
   * Return the next step size to take for a given function domain argument.
   * 
   * @param x the value of the current location at which the next step size is sought
   * 
   * @return the size of the next step to take (x + step) would be the value evaluated next.
   */
  public double step(double x);

}
