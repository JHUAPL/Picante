package picante.spice.adapters;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;
import picante.spice.provided.InertialFrames;

/**
 * Adapter class that adapts a built-in inertial frame to the state transform function interface.
 */
class InertialFrameAdapter implements StateTransformFunction {

  private final FrameID fromID;
  private final FrameID toID;
  private final InertialFrames inertialFrame;

  public InertialFrameAdapter(FrameID fromID, FrameID toID, InertialFrames inertialFrame) {
    this.fromID = fromID;
    this.toID = toID;
    this.inertialFrame = inertialFrame;
  }

  @Override
  public Coverage getCoverage() {
    /*
     * Inertial frames are valid for all time.
     */
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
    return inertialFrame.getTransform(time, buffer);
  }

  @Override
  public StateTransform getStateTransform(double time, StateTransform buffer) {
    buffer = buffer == null ? new StateTransform() : buffer;
    return inertialFrame.getStateTransform(time, buffer);
  }

}
