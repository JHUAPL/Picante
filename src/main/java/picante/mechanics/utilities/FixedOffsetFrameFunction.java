package picante.mechanics.utilities;

import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.FrameEvaluationException;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;

/**
 * Simple implementation of the {@link StateTransformFunction} interface that copies the contents of
 * a rotation matrix and defines a time invariant state transform between two frames.
 */
@Deprecated
public final class FixedOffsetFrameFunction implements StateTransformFunction {

  /**
   * The {@link FrameID} of the frame to transform states from.
   */
  private final FrameID fromID;

  /**
   * The {@link FrameID} of the frame to transform states to.
   */
  private final FrameID toID;

  /**
   * The {@link Coverage} for which this transformation is valid.
   */
  private final Coverage coverage;

  /**
   * The unwritable buffer which caches the rotation contents.
   */
  private final UnwritableRotationMatrixIJK matrix;

  /**
   * Creates a time invariant {@link StateTransformFunction} valid for {@link Coverage#ALL_TIME}
   * connecting one frame to another.
   * 
   * @param fromID the {@link FrameID} of the frame from which states will be transformed from
   * @param toID the {@link FrameID} of the frame from which states will be transformed to
   * @param rotation the rotation matrix that rotates states from <code>fromID</code> to
   *        <code>toID</code>
   */
  public FixedOffsetFrameFunction(FrameID fromID, FrameID toID,
      UnwritableRotationMatrixIJK rotation) {
    this(fromID, toID, Coverage.ALL_TIME, rotation);
  }

  /**
   * Creates a time invariant {@link StateTransformFunction} connecting one frame to another.
   * 
   * @param fromID the {@link FrameID} of the frame from which states will be transformed from
   * @param toID the {@link FrameID} of the frame from which states will be transformed to
   * @param coverage the coverage used to define the time of validity of the transform
   * @param rotation the rotation matrix that rotates states from <code>fromID</code> to
   *        <code>toID</code>
   */
  public FixedOffsetFrameFunction(FrameID fromID, FrameID toID, Coverage coverage,
      UnwritableRotationMatrixIJK rotation) {
    this.matrix = new UnwritableRotationMatrixIJK(rotation);
    this.fromID = fromID;
    this.toID = toID;
    this.coverage = coverage;
  }

  @Override
  public FrameID getFromID() {
    return fromID;
  }

  @Override
  public FrameID getToID() {
    return toID;
  }

  @Override
  public Coverage getCoverage() {
    return coverage;
  }

  @Override
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {

    if (!coverage.contains(time)) {
      throw new FrameEvaluationException("Unable to evaluate frame transform at time: " + time
          + ". It lies outside the reported coverage.");
    }

    return buffer.setTo(matrix);
  }

  @Override
  public StateTransform getStateTransform(double time, StateTransform buffer) {

    if (!coverage.contains(time)) {
      throw new FrameEvaluationException("Unable to evaluate state transform at time: " + time
          + ". It lies outside the reported coverage.");
    }

    buffer.setRotation(matrix);
    buffer.setRotationDerivative(MatrixIJK.ZEROS);
    return buffer;
  }

}
