package picante.spice.kernel.tk.fk.dynamic;

import java.util.Map;

import com.google.common.base.Preconditions;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;
import picante.mechanics.StateVector;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.spice.GeneralAberratedEphemerisProvider;

public class DynamicConstantVectorFunction implements DefinableStateVectorFunction {
  final UnwritableVectorIJK vector;
  final int frameCode;
  final int relativeCode;
  private StateTransformFunction function;
  private boolean defined = false;

  public DynamicConstantVectorFunction(UnwritableVectorIJK vector, int frameCode,
      int relativeCode) {
    super();
    this.vector = vector;
    this.frameCode = frameCode;
    this.relativeCode = relativeCode;
  }


  @Override
  public UnwritableVectorIJK getVector(double time) {
    Preconditions.checkState(defined,
        "You must call the define(...) method before calling getVector().");
    RotationMatrixIJK rotMat = function.getTransform(time, new RotationMatrixIJK());
    return rotMat.mxv(vector);
  }


  @Override
  public StateVector getStateVector(double time) {
    Preconditions.checkState(defined,
        "You must call the define(...) method before calling getStateVector().");
    StateVector stateVec = new StateVector(vector, VectorIJK.ZERO);
    StateTransform stateTrans = function.getStateTransform(time, new StateTransform());
    return stateTrans.mxv(stateVec);
  }

  @Override
  public void define(GeneralAberratedEphemerisProvider generalProvider,
      Map<Integer, FrameID> frameIDMap,
      @SuppressWarnings("unused") Map<Integer, EphemerisID> ephemerisIDMap)
      throws DynamicFrameDefinitionException {
    FrameID frameID = IDUtilities.code2FrameID(frameCode, frameIDMap);
    FrameID relativeID = IDUtilities.code2FrameID(relativeCode, frameIDMap);
    AberratedEphemerisProvider provider = generalProvider.getProvider(false);
    function = provider.createStateTransformFunction(frameID, relativeID, Coverage.ALL_TIME);
    defined = true;
  }

}
