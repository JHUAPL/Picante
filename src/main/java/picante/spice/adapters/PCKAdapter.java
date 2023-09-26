package picante.spice.adapters;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;
import picante.spice.kernel.tk.pck.PCKFrameFunction;

class PCKAdapter implements StateTransformFunction {

  private final FrameID fromID;
  private final FrameID toID;
  private final PCKFrameFunction function;

  public PCKAdapter(FrameID fromID, FrameID toID, PCKFrameFunction function) {
    this.fromID = fromID;
    this.toID = toID;
    this.function = function;
  }

  @Override
  public Coverage getCoverage() {
    return Coverage.ALL_TIME;
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
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {

    buffer = buffer == null ? new RotationMatrixIJK() : buffer;

    function.getTransform(time, buffer);

    /*
     * Invert the resultant frame transformation as the PCK system naturally generates inertial to
     * body-fixed transformations, not the reverse.
     */
    return buffer.transpose();
  }

  @Override
  public StateTransform getStateTransform(double time, StateTransform buffer) {

    buffer = buffer == null ? new StateTransform() : buffer;

    function.getStateTransform(time, buffer);

    /*
     * Invert the resultant state transformation as the PCK system naturally generates inertial to
     * body-fixed transformations, not the reverse.
     */
    return buffer.invert();
  }

}
