package picante.mechanics;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;

/**
 * Simple implementation of the <code>FrameTransformFunction</code> interface that supplies a
 * constant rotation matrix over all supplied times.
 */
public class ConstantFrameTransformFunction implements FrameTransformFunction {

  private final RotationMatrixIJK matrix = new RotationMatrixIJK();

  private final Coverage coverage;

  private final FrameTestCodes fromID;

  private final FrameTestCodes toID;

  public ConstantFrameTransformFunction(FrameTestCodes fromID, FrameTestCodes toID,
      Coverage coverage, UnwritableRotationMatrixIJK matrix) {
    this.coverage = coverage;
    this.fromID = fromID;
    this.toID = toID;
    this.matrix.setTo(matrix);
  }

  @Override
  public Coverage getCoverage() {
    return coverage;
  }

  @Override
  public FrameTestCodes getFromID() {
    return fromID;
  }

  @Override
  public FrameTestCodes getToID() {
    return toID;
  }

  @Override
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {

    buffer = buffer == null ? new RotationMatrixIJK() : buffer;

    if (!coverage.contains(time)) {
      throw new FrameEvaluationException();
    }

    return buffer.setTo(matrix);
  }

}
