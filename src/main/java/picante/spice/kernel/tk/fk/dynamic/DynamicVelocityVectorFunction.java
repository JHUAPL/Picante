package picante.spice.kernel.tk.fk.dynamic;

import java.util.Map;

import com.google.common.base.Preconditions;
import picante.exceptions.BugException;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.ForwardingStateVectorFunction;
import picante.mechanics.FrameID;
import picante.mechanics.StateVector;
import picante.mechanics.StateVectorFunction;
import picante.mechanics.StateVectorFunctions;
import picante.mechanics.UnwritableStateVector;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.spice.GeneralAberratedEphemerisProvider;
import picante.spice.kernelpool.content.SpiceAberrationCorrection;

public class DynamicVelocityVectorFunction implements DefinableStateVectorFunction {

  private final int observerCode;
  private final int targetCode;
  private final int frameCode;
  private StateVectorFunction velocityAccelerationFunction;
  private boolean defined = false;
  private final SpiceAberrationCorrection abCorr;

  static class UnitizedVelocityFunction extends ForwardingStateVectorFunction {

    UnitizedVelocityFunction(StateVectorFunction delegate) {
      super(delegate);
    }

    @Override
    public StateVector getState(double time, StateVector buffer) {
      delegate.getState(time, buffer);
      /*
       * The position components of this state should never be used.
       */
      buffer.getVelocity().unitize();
      return buffer;
    }

    @SuppressWarnings("unused")
    @Override
    public VectorIJK getPosition(double time, VectorIJK buffer) {
      throw new BugException("Method should never be called.");
    }

  }

  public DynamicVelocityVectorFunction(int observerCode, int targetCode, int frameCode,
      SpiceAberrationCorrection abCorr) {
    Preconditions.checkNotNull(observerCode);
    Preconditions.checkNotNull(targetCode);
    Preconditions.checkNotNull(frameCode);
    this.observerCode = observerCode;
    this.targetCode = targetCode;
    this.frameCode = frameCode;
    this.abCorr = abCorr;
  }


  @Override
  public void define(GeneralAberratedEphemerisProvider generalProvider,
      Map<Integer, FrameID> frameIDMap, Map<Integer, EphemerisID> ephemerisIDMap)
      throws DynamicFrameDefinitionException {
    FrameID frameID = IDUtilities.code2FrameID(frameCode, frameIDMap);
    EphemerisID observerID = IDUtilities.code2EphemerisID(observerCode, ephemerisIDMap);
    EphemerisID targetID = IDUtilities.code2EphemerisID(targetCode, ephemerisIDMap);
    AberratedEphemerisProvider provider = generalProvider.getProvider(abCorr.isTriple());
    UnitizedVelocityFunction normalFunction =
        new UnitizedVelocityFunction(provider.createAberratedStateVectorFunction(targetID,
            observerID, frameID, Coverage.ALL_TIME, abCorr.getAbCorr()));
    velocityAccelerationFunction = StateVectorFunctions.createQuadraticDerivative(normalFunction);
    defined = true;
  };

  @Override
  public UnwritableVectorIJK getVector(double time) {
    Preconditions.checkState(defined,
        "You must call the define(...) method before calling getVector().");
    StateVector vector = velocityAccelerationFunction.getState(time, new StateVector());
    return vector.getPosition();
  }

  @Override
  public UnwritableStateVector getStateVector(double time) {
    Preconditions.checkState(defined,
        "You must call the define(...) method before calling getStateVector().");
    return velocityAccelerationFunction.getState(time, new StateVector());
  }
}
