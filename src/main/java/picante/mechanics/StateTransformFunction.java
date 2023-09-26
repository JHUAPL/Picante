package picante.mechanics;

/**
 * Interface that describes a function that computes state transformations from one frame to another
 * at the requested time. As state transformations are merely frame transformations with temporal
 * derivative information, this interface inherits directly from the frame transform one.
 */
public interface StateTransformFunction extends FrameTransformFunction {

  /**
   * Obtain the state transform between the <code>fromID</code> and the <code>toID</code> at the
   * requested time.
   * 
   * @param time the time at which the state transform is to be computed. This should be contained
   *        within the coverage reported by the function via the
   *        {@link FrameTransformFunction#getCoverage()} method.
   * 
   * @return the resultant state transform.
   * 
   * @throws FrameEvaluationException a runtime exception, if anything goes awry in the computation
   *         of the state transform. Implementors are encouraged to subclass this exception for
   *         their own purposes.
   */
  public default StateTransform getStateTransform(double time) {
    return getStateTransform(time, new StateTransform());
  };

  /**
   * Obtain the state transform between the <code>fromID</code> and the <code>toID</code> at the
   * requested time.
   * 
   * @param time the time at which the state transform is to be computed. This should be contained
   *        within the coverage reported by the function via the
   *        {@link FrameTransformFunction#getCoverage()} method.
   * 
   * @param buffer a buffer to capture the resultant state transform, or null if the underlying
   *        implementation is to create a new instance and return it instead.
   * 
   * @return a reference to buffer for convenience.
   * 
   * @throws FrameEvaluationException a runtime exception, if anything goes awry in the computation
   *         of the state transform. Implementors are encouraged to subclass this exception for
   *         their own purposes.
   */
  public StateTransform getStateTransform(double time, StateTransform buffer);

}
