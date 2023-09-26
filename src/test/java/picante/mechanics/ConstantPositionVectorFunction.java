package picante.mechanics;

import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * Simple implementation of the <code>PositionVectorFunction</code> interface that supplies a
 * constant position vector over all supplied times.
 */
public class ConstantPositionVectorFunction implements PositionVectorFunction {

  private final VectorIJK vector = new VectorIJK();

  private final Coverage coverage;

  private final EphemerisTestCodes targetID;

  private final EphemerisTestCodes observerID;

  private final FrameTestCodes frameID;

  public ConstantPositionVectorFunction(EphemerisTestCodes targetID, EphemerisTestCodes observerID,
      FrameTestCodes frameID, Coverage coverage, UnwritableVectorIJK vector) {
    this.coverage = coverage;
    this.targetID = targetID;
    this.observerID = observerID;
    this.frameID = frameID;
    this.vector.setTo(vector);
  }

  @Override
  public VectorIJK getPosition(double time, VectorIJK buffer) {
    buffer = buffer == null ? new VectorIJK() : buffer;
    if (!coverage.contains(time)) {
      throw new EphemerisEvaluationException();
    }
    return buffer.setTo(vector);
  }

  @Override
  public Coverage getCoverage() {
    return coverage;
  }

  @Override
  public FrameTestCodes getFrameID() {
    return frameID;
  }

  @Override
  public EphemerisTestCodes getObserverID() {
    return observerID;
  }

  @Override
  public EphemerisTestCodes getTargetID() {
    return targetID;
  }

}
