package picante.spice.adapters;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;
import picante.spice.kernel.pck.PCKSegment;

class BinaryPCKSegmentAdapter implements StateTransformFunction {

  private final FrameID fromID;

  private final FrameID toID;

  private final PCKSegment segment;

  protected BinaryPCKSegmentAdapter(FrameID fromID, FrameID toID, PCKSegment segment) {
    this.fromID = fromID;
    this.toID = toID;
    this.segment = segment;
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
    return segment.getCoverage();
  }

  @Override
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
    buffer = buffer == null ? new RotationMatrixIJK() : buffer;
    segment.getTransform(time, buffer);

    /*
     * Invert the resultant transformation, since the binary PCK provides a transform that takes
     * vectors from the toID frame (reference) to the fromID frame (body).
     */
    return buffer.transpose();
  }

  @Override
  public StateTransform getStateTransform(double time, StateTransform buffer) {
    buffer = buffer == null ? new StateTransform() : buffer;
    segment.getTransform(time, buffer);

    /*
     * Invert the resultant transformation, since the binary PCK provides a transform that takes
     * vectors from the toID frame (reference) to the fromID frame (body).
     */
    return buffer.invert();
  }

}
