package picante.spice.kernel.tk.fk.dynamic;

import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.spice.GeneralAberratedEphemerisProvider;

public class DynamicFrozenFrameFunction implements DynamicFrameFunction {
  private final DynamicFrameFunction dynamicFrameFunction;
  private final double freezeEpoch;
  private StateTransform stateTransform;
  private final AtomicBoolean stateTransformIsSet = new AtomicBoolean(false);

  public DynamicFrozenFrameFunction(DynamicFrameFunction dynamicFrameFunction, double freezeEpoch) {
    super();
    this.dynamicFrameFunction = dynamicFrameFunction;
    this.freezeEpoch = freezeEpoch;
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
  public RotationMatrixIJK getTransform(@SuppressWarnings("unused") double time,
      RotationMatrixIJK buffer) {
    if (!stateTransformIsSet.get()) {
      stateTransform = dynamicFrameFunction.getStateTransform(freezeEpoch, new StateTransform());
      stateTransformIsSet.set(true);
    }
    return buffer.setTo(stateTransform.getRotation());
  }

  @Override
  public StateTransform getStateTransform(@SuppressWarnings("unused") double time,
      StateTransform buffer) {
    if (!stateTransformIsSet.get()) {
      stateTransform = dynamicFrameFunction.getStateTransform(freezeEpoch, new StateTransform());
      stateTransformIsSet.set(true);
    }
    return buffer.setTo(stateTransform);
  }
}
