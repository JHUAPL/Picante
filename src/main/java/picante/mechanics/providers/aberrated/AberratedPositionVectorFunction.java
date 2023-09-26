package picante.mechanics.providers.aberrated;

import picante.mechanics.PositionVectorFunction;

/**
 * Extension of the {@link PositionVectorFunction} interface that provides a method to compute the
 * light time to the target. When receiving an instance of this method, it is safe to assume that
 * the positions returned from the
 * {@link PositionVectorFunction#getPosition(double, picante.math.vectorspace.VectorIJK)}
 * include the corrections described by {@link AberratedPositionVectorFunction#getCorrection()}
 */
public interface AberratedPositionVectorFunction extends PositionVectorFunction, Aberrated {

  /**
   * Computes the one-way light time for a the observer and target, per the correction described by
   * {@link AberratedPositionVectorFunction#getCorrection()}.
   * 
   * @param time the time at {@link AberratedPositionVectorFunction#getObserverID()} at which to
   *        compute the one way light time
   * 
   * @return the one-way light time, which is always a positive number. In the transmission case,
   *         this is the amount to add to time at which a signal departing from
   *         {@link AberratedPositionVectorFunction#getObserverID()} arrives at
   *         {@link AberratedPositionVectorFunction#getTargetID()}. In the receipt case, this is the
   *         amount to subtract to determine the time at which a signal departing the target arrives
   *         at the observer.
   */
  public double getLightTime(double time);

}
