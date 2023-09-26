package picante.spice.kernel.tk.fk.dynamic;

import java.util.Map;

import com.google.common.base.Preconditions;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.UnwritableStateVector;
import picante.spice.GeneralAberratedEphemerisProvider;

public class DynamicTwoVectorFrameFunction implements DynamicFrameFunction {
  private final int fromID;
  private final int toID;
  private final DefinableStateVectorFunction primaryFunction;
  private final DefinableStateVectorFunction secondaryFunction;
  private final TwoVectorMatrix twoVectorMatrix;
  private final double angleSepTolerance;
  private boolean defined = false;

  public DynamicTwoVectorFrameFunction(int fromID, int toID,
      DefinableStateVectorFunction primaryFunction, DefinableStateVectorFunction secondaryFunction,
      TwoVectorMatrix twoVectorMatrix, double angleSepTolerance) {
    super();
    Preconditions.checkNotNull(fromID);
    Preconditions.checkNotNull(toID);
    this.fromID = fromID;
    this.toID = toID;
    this.primaryFunction = primaryFunction;
    this.secondaryFunction = secondaryFunction;
    this.twoVectorMatrix = twoVectorMatrix;
    this.angleSepTolerance = angleSepTolerance;
  }

  @Override
  public void define(GeneralAberratedEphemerisProvider generalProvider,
      Map<Integer, FrameID> frameIDMap, Map<Integer, EphemerisID> ephemerisIDMap)
      throws DynamicFrameDefinitionException {
    primaryFunction.define(generalProvider, frameIDMap, ephemerisIDMap);
    secondaryFunction.define(generalProvider, frameIDMap, ephemerisIDMap);
    defined = true;
  }

  @Override
  public int getFromID() {
    return fromID;
  }

  @Override
  public int getToID() {
    return toID;
  }

  @Override
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
    Preconditions.checkState(defined,
        "You must call the define(...) method before calling getTransform().");
    UnwritableVectorIJK primaryVector = primaryFunction.getVector(time);
    UnwritableVectorIJK secondaryVector = secondaryFunction.getVector(time);
    // System.out.println("primary " + primaryVector + " at time " + time);
    if (primaryVector.getSeparation(secondaryVector) < angleSepTolerance) {
      throw new RuntimeException("Two-vector frame from " + fromID + " to " + toID
          + " cannot be computed: the angular separation between vectors is beneath the allowed tolerance. "
          + primaryVector.getSeparation(secondaryVector) + " < " + angleSepTolerance);
    }
    RotationMatrixIJK rotation = twoVectorMatrix.makeMatrix(primaryVector, secondaryVector);
    return buffer.setTo(rotation);
  }

  @Override
  public StateTransform getStateTransform(double time, StateTransform buffer) {
    Preconditions.checkState(defined,
        "You must call the define(...) method before calling getStateTransform().");
    UnwritableStateVector primaryStateVector = primaryFunction.getStateVector(time);
    UnwritableStateVector secondaryStateVector = secondaryFunction.getStateVector(time);
    if (primaryStateVector.getSeparation(secondaryStateVector) < angleSepTolerance) {
      throw new RuntimeException("Two-vector frame from " + fromID + " to " + toID
          + " cannot be computed: the angular separation between vectors is beneath the allowed tolerance. "
          + primaryStateVector.getSeparation(secondaryStateVector) + " < " + angleSepTolerance);
    }
    StateTransform stateTransform =
        twoVectorMatrix.makeStateTransform(primaryStateVector, secondaryStateVector);
    return buffer.setTo(stateTransform);
  }
}
