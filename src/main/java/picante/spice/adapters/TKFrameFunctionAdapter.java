package picante.spice.adapters;

import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;
import picante.spice.kernel.tk.fk.TKFrameFunction;

/**
 * Adapter that converts {@link TKFrameFunction} into a crucible {@link StateTransformFunction}.
 * <p>
 * Per the NAIF specification, these functions are valid for all time.
 * </p>
 */
class TKFrameFunctionAdapter implements StateTransformFunction {

  private final FrameID fromID;
  private final FrameID toID;
  private final TKFrameFunction function;

  public TKFrameFunctionAdapter(FrameID fromID, FrameID toID, TKFrameFunction function) {
    this.function = function;
    this.fromID = fromID;
    this.toID = toID;
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
    return Coverage.ALL_TIME;
  }

  @Override
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
    buffer = buffer == null ? new RotationMatrixIJK() : buffer;
    function.getTransform(time, buffer);
    return buffer;
  }

  @Override
  public StateTransform getStateTransform(double time, StateTransform buffer) {
    buffer = buffer == null ? new StateTransform() : buffer;
    buffer.setRotationDerivative(MatrixIJK.ZEROS);
    function.getTransform(time, buffer.getRotation());
    return buffer;
  }

}
