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
import picante.spice.kernelpool.content.SpiceAberrationCorrection;

public class DynamicAberratedConstantVectorFunction implements DefinableStateVectorFunction {
  private final UnwritableVectorIJK vector;
  private final int frameCode;
  private final int relativeCode;
  private final int observerCode;
  private StateTransformFunction function;
  private boolean defined = false;
  private SpiceAberrationCorrection abCorr;

  public DynamicAberratedConstantVectorFunction(DynamicConstantVectorFunction constantVec,
      SpiceAberrationCorrection abCorr, int observerCode) {
    super();
    this.vector = constantVec.vector;
    this.frameCode = constantVec.frameCode;
    this.relativeCode = constantVec.relativeCode;
    this.abCorr = abCorr;
    this.observerCode = observerCode;
    /*
     * 
     * explanation shown in zzdynfrm Can only accept receipt LT or CN, transmission XLT or XCN, or
     * stellar S or XS transmission correctedTime = time + lt (XLT, XCN) reception correctedTime =
     * time - lt (LT, CN) (S XS, no clue)
     */
  }

  public UnwritableVectorIJK getVector() {
    return vector;
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
      Map<Integer, FrameID> frameIDMap, Map<Integer, EphemerisID> ephemerisIDMap)
      throws DynamicFrameDefinitionException {
    FrameID frameID = IDUtilities.code2FrameID(frameCode, frameIDMap);
    FrameID relativeID = IDUtilities.code2FrameID(relativeCode, frameIDMap);
    @SuppressWarnings("unused")
    EphemerisID observerID = IDUtilities.code2EphemerisID(observerCode, ephemerisIDMap);
    AberratedEphemerisProvider provider = generalProvider.getProvider(abCorr.isTriple());
    // TODO Needs fixing
    function = provider.createStateTransformFunction(frameID, relativeID, Coverage.ALL_TIME);
    defined = true;
  }

}
