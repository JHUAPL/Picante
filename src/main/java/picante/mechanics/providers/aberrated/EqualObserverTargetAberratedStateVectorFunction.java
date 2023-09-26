package picante.mechanics.providers.aberrated;

import static com.google.common.base.Preconditions.checkNotNull;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateVector;
import picante.mechanics.utilities.AbstractPositionVectorFunction;

/**
 * This class exists just to inject zeros into the appropriate places, in the event that the center
 * of a frame is coincident with the observer requested.
 */
class EqualObserverTargetAberratedStateVectorFunction extends AbstractPositionVectorFunction
    implements AberratedStateVectorFunction {

  private final AberrationCorrection correction;

  EqualObserverTargetAberratedStateVectorFunction(EphemerisID targetID, EphemerisID observerID,
      FrameID frameID, Coverage coverage, AberrationCorrection correction) {
    super(targetID, observerID, frameID, coverage);
    this.correction = checkNotNull(correction);
  }

  @Override
  public double getLightTime(@SuppressWarnings("unused") double time) {
    return 0;
  }

  @Override
  public VectorIJK getPosition(@SuppressWarnings("unused") double time, VectorIJK buffer) {
    buffer = buffer == null ? new VectorIJK() : buffer;
    return buffer.setTo(VectorIJK.ZERO);
  }

  @Override
  public AberrationCorrection getCorrection() {
    return correction;
  }

  @Override
  public StateVector getState(@SuppressWarnings("unused") double time, StateVector buffer) {
    buffer = buffer == null ? new StateVector() : buffer;
    return buffer.setTo(StateVector.ZERO);
  }

  @Override
  public double getLightTimeDerivative(@SuppressWarnings("unused") double time) {
    return 0;
  }

}
