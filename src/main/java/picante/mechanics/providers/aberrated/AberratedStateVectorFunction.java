package picante.mechanics.providers.aberrated;

import picante.mechanics.StateVectorFunction;

/**
 * Extension of the {@link StateVectorFunction} interface that provides a method to compute the
 * light time to the target. When receiving an instance of this method, it is safe to assume that
 * the positions returned from the
 * {@link StateVectorFunction#getPosition(double, picante.math.vectorspace.VectorIJK)} or
 * states from {@link StateVectorFunction#getState(double, picante.mechanics.StateVector)}
 * include the corrections described by {@link AberratedPositionVectorFunction#getCorrection()}
 */
public interface AberratedStateVectorFunction
    extends AberratedPositionVectorFunction, StateVectorFunction, Aberrated {

  /**
   * Computes the derivative of the one-way light time for a the observer and target, per the
   * correction described by {@link AberratedPositionVectorFunction#getCorrection()}.
   * 
   * @param time the time at {@link AberratedPositionVectorFunction#getObserverID()} at which to
   *        compute the one way light time
   * 
   * @return the derivative of the one-way light time
   * 
   * @see AberratedPositionVectorFunction#getLightTime(double)
   */
  public double getLightTimeDerivative(double time);

}
