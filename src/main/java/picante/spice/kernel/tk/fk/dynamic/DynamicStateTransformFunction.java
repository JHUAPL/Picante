package picante.spice.kernel.tk.fk.dynamic;

import java.util.Map;

import com.google.common.base.Preconditions;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;
import picante.spice.GeneralAberratedEphemerisProvider;

public class DynamicStateTransformFunction implements StateTransformFunction, Defineable {
  private final DynamicFrameFunction dynamicFrameFunction;
  private final FrameID fromID;
  private final FrameID toID;
  private boolean defined = false;

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

  public DynamicFrameFunction getDynamicFrameFunction() {
    return dynamicFrameFunction;
  }

  public DynamicStateTransformFunction(DynamicFrameFunction dynamicFrameFunction, FrameID fromID,
      FrameID toID) {
    super();
    Preconditions.checkNotNull(fromID);
    Preconditions.checkNotNull(toID);
    this.dynamicFrameFunction = dynamicFrameFunction;
    this.fromID = fromID;
    this.toID = toID;
  }

  @Override
  public void define(GeneralAberratedEphemerisProvider generalProvider,
      Map<Integer, FrameID> frameIDMap, Map<Integer, EphemerisID> ephemerisIDMap)
      throws DynamicFrameDefinitionException {
    dynamicFrameFunction.define(generalProvider, frameIDMap, ephemerisIDMap);
    defined = true;
  }

  @Override
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
    Preconditions.checkState(defined, "The dynamic frame from " + fromID + " to " + toID
        + " could not be defined and is unusable.");
    return dynamicFrameFunction.getTransform(time, buffer);
  }

  @Override
  public StateTransform getStateTransform(double time, StateTransform buffer) {
    Preconditions.checkState(defined, "The dynamic frame from " + fromID + " to " + toID
        + " could not be defined and is unusable.");
    return dynamicFrameFunction.getStateTransform(time, buffer);
  }
}
