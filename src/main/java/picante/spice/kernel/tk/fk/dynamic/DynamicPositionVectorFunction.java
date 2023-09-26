package picante.spice.kernel.tk.fk.dynamic;

import java.util.Map;

import com.google.common.base.Preconditions;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateVector;
import picante.mechanics.StateVectorFunction;
import picante.mechanics.UnwritableStateVector;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.spice.GeneralAberratedEphemerisProvider;
import picante.spice.kernelpool.content.SpiceAberrationCorrection;

public class DynamicPositionVectorFunction implements DefinableStateVectorFunction {
  private final int observerCode;
  private final int targetCode;
  private final int relativeCode;
  private StateVectorFunction function;
  private boolean defined = false;
  private final SpiceAberrationCorrection abCorr;

  public DynamicPositionVectorFunction(int observerCode, int targetCode, int relativeCode,
      SpiceAberrationCorrection abCorr) {
    Preconditions.checkNotNull(observerCode);
    Preconditions.checkNotNull(targetCode);
    Preconditions.checkNotNull(relativeCode);
    this.observerCode = observerCode;
    this.targetCode = targetCode;
    this.relativeCode = relativeCode;
    this.function = null;
    this.abCorr = abCorr;
  }


  @Override
  public void define(GeneralAberratedEphemerisProvider generalProvider,
      Map<Integer, FrameID> frameIDMap, Map<Integer, EphemerisID> ephemerisIDMap)
      throws DynamicFrameDefinitionException {
    FrameID relativeID = IDUtilities.code2FrameID(relativeCode, frameIDMap);
    EphemerisID observerID = IDUtilities.code2EphemerisID(observerCode, ephemerisIDMap);
    EphemerisID targetID = IDUtilities.code2EphemerisID(targetCode, ephemerisIDMap);
    AberratedEphemerisProvider provider = generalProvider.getProvider(abCorr.isTriple());
    function = provider.createAberratedStateVectorFunction(targetID, observerID, relativeID,
        Coverage.ALL_TIME, abCorr.getAbCorr());
    defined = true;
  };

  @Override
  public UnwritableVectorIJK getVector(double time) {
    Preconditions.checkState(defined,
        "You must call the define(...) method before calling getVector().");
    return function.getPosition(time, new VectorIJK());
  }

  @Override
  public UnwritableStateVector getStateVector(double time) {
    Preconditions.checkState(defined,
        "You must call the define(...) method before calling getStateVector().");
    return function.getState(time, new StateVector());
  }


}
