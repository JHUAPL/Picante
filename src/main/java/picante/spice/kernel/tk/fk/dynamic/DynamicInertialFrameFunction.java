package picante.spice.kernel.tk.fk.dynamic;

import java.util.Map;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.spice.GeneralAberratedEphemerisProvider;

public class DynamicInertialFrameFunction implements DynamicFrameFunction {
  private final DynamicFrameFunction dynamicFrameFunction;

  public DynamicInertialFrameFunction(DynamicFrameFunction dynamicFrameFunction) {
    super();
    this.dynamicFrameFunction = dynamicFrameFunction;
  }

  @Override
  public void define(GeneralAberratedEphemerisProvider generalProvider,
      Map<Integer, FrameID> frameIDMap, Map<Integer, EphemerisID> ephemerisIDMap)
      throws DynamicFrameDefinitionException {
    dynamicFrameFunction.define(generalProvider, frameIDMap, ephemerisIDMap);
  }

  @Override
  public int getFromID() {
    return dynamicFrameFunction.getFromID();
  }

  @Override
  public int getToID() {
    return dynamicFrameFunction.getToID();
  }

  @Override
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
    return buffer.setTo(dynamicFrameFunction.getTransform(time, buffer));
  }

  @Override
  public StateTransform getStateTransform(double time, StateTransform buffer) {
    StateTransform stateTransform = dynamicFrameFunction.getStateTransform(time, buffer);
    return buffer.setTo(new StateTransform(stateTransform.getRotation(), MatrixIJK.ZEROS));
  }
}
