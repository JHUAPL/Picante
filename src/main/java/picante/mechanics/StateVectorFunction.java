package picante.mechanics;

/**
 * Interface that describes a function that computes the state, position and velocity, of a target
 * relative to an observer in a specific frame at the requested time. As states are merely positions
 * with temporal derivative information, this interface inherits directly from the position one. As
 * with the position function, the vector tail is at the observer and the head at the target.
 */
public interface StateVectorFunction extends PositionVectorFunction {

  /**
   * Obtain the state of the <code>targetID</code> relative to the <code>observerID</code> in the
   * <code>frameID</code> frame at the requested time.
   * 
   * @param time the time at which the state vector is to be computed. This should be contained
   *        within the coverage reported by the function via the
   *        {@link PositionVectorFunction#getCoverage()} method.
   * 
   * @return the resultant state vector.
   * 
   * @throws EphemerisEvaluationException a runtime exception, if anything goes awry in the
   *         computation of the state transform. Implementors are encouraged to subclass this
   *         exception for their own purposes.
   */
  public default StateVector getState(double time) {
    return getState(time, new StateVector());
  }

  /**
   * Obtain the state of the <code>targetID</code> relative to the <code>observerID</code> in the
   * <code>frameID</code> frame at the requested time.
   * 
   * @param time the time at which the state vector is to be computed. This should be contained
   *        within the coverage reported by the function via the
   *        {@link PositionVectorFunction#getCoverage()} method.
   * 
   * @param buffer a buffer to capture the resultant state vector, or null if the underlying
   *        implementation is to create a new instance and return it instead.
   * 
   * @return a reference to buffer for convenience.
   * 
   * @throws EphemerisEvaluationException a runtime exception, if anything goes awry in the
   *         computation of the state transform. Implementors are encouraged to subclass this
   *         exception for their own purposes.
   */
  public StateVector getState(double time, StateVector buffer);

}
