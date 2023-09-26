package picante.mechanics;

import picante.math.vectorspace.RotationMatrixIJK;

/**
 * Interface that describes a function that computes the frame transformation from one frame to
 * another at the requested time. The frame transform rotates vectors from an originating frame,
 * designated by the <code>fromID</code> to a destination frame, designated by the <code>toID</code>
 */
public interface FrameTransformFunction {

  /**
   * Retrieves the frame ID of the originating frame.
   * 
   * @return the ID
   */
  public FrameID getFromID();

  /**
   * Retrieves the frame ID of the destination frame.
   * 
   * @return the ID
   */
  public FrameID getToID();

  /**
   * Retrieves the coverage over which the frame transformation is valid.
   * 
   * @return the coverage
   */
  public Coverage getCoverage();

  /**
   * Obtain the frame transform between the <code>fromID</code> and the <code>toID</code> at the
   * requested time.
   * 
   * @param time the time at which the frame transform is to be computed. This should be contained
   *        within the coverage reported by the function via the
   *        {@link FrameTransformFunction#getCoverage()} method.
   * 
   * @return the result frame transformation.
   * 
   * @throws FrameEvaluationException a runtime exception, if anything goes awry in the computation
   *         of the frame transform. Implementors are encouraged to subclass this exception for
   *         their own purposes
   */
  public default RotationMatrixIJK getTransform(double time) {
    return getTransform(time, new RotationMatrixIJK());
  }

  /**
   * Obtain the frame transform between the <code>fromID</code> and the <code>toID</code> at the
   * requested time.
   * 
   * @param time the time at which the frame transform is to be computed. This should be contained
   *        within the coverage reported by the function via the
   *        {@link FrameTransformFunction#getCoverage()} method.
   * 
   * @param buffer a buffer to capture the result frame transformation, or null if the underlying
   *        implementation is to create a new instance and return it instead.
   * 
   * @return a reference to buffer for convenience.
   * 
   * @throws FrameEvaluationException a runtime exception, if anything goes awry in the computation
   *         of the frame transform. Implementors are encouraged to subclass this exception for
   *         their own purposes
   */
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer);

}
