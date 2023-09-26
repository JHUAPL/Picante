package picante.mechanics;

import picante.math.vectorspace.VectorIJK;

/**
 * Interface that describes a function that computes the position of a target body relative to an
 * observer in a specified frame at a particular time. The position function supplies a vector whose
 * tail is at the observer and head is at the target, like so:
 * 
 * <pre>
 * <code>
 * OBSERVER --------------&gt; TARGET
 * </pre>
 * 
 * </code> TODO: (added by JonV) talk to Scott about possibly separating out the identity stuff from
 * the function stuff
 */
public interface PositionVectorFunction {

  /**
   * Retrieves the ephemeris ID of the observer object.
   * 
   * @return the ID
   */
  public EphemerisID getObserverID();

  /**
   * Retrieves the ephemeris ID of the target object.
   * 
   * @return the ID
   */
  public EphemerisID getTargetID();

  /**
   * Retrieves the frame ID of the frame in which the vector from the observer to the target is
   * computed.
   * 
   * @return the ID
   */
  public FrameID getFrameID();

  /**
   * Retrieves the coverage over which the position vector function is valid.
   * 
   * @return the coverage
   */
  public Coverage getCoverage();

  /**
   * Obtain the position vector connecting the <code>observerID</code> to the <code>targetID</code>
   * in the specified <code>frameID</code> at the requested time.
   * 
   * @param time the time at which the position vector is to be computed. This should be contained
   *        within the coverage reported by the function via the
   *        {@link PositionVectorFunction#getCoverage()} method.
   * 
   * @return the resultant position vector.
   * 
   * @throws EphemerisEvaluationException a runtime exception, if anything goes awry in the
   *         computation of the position vector. Implementors are encouraged to subclass this
   *         exception for their own purposes.
   */
  public default VectorIJK getPosition(double time) {
    return getPosition(time, new VectorIJK());
  }

  /**
   * Obtain the position vector connecting the <code>observerID</code> to the <code>targetID</code>
   * in the specified <code>frameID</code> at the requested time.
   * 
   * @param time the time at which the position vector is to be computed. This should be contained
   *        within the coverage reported by the function via the
   *        {@link PositionVectorFunction#getCoverage()} method.
   * 
   * @param buffer a buffer to capture the resultant position vector, or null if the underlying
   *        implementation is to create a new instance and return it instead.
   * 
   * @return a reference to buffer for convenience.
   * 
   * @throws EphemerisEvaluationException a runtime exception, if anything goes awry in the
   *         computation of the position vector. Implementors are encouraged to subclass this
   *         exception for their own purposes.
   */
  public VectorIJK getPosition(double time, VectorIJK buffer);

}
