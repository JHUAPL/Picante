package picante.mechanics.providers.aberrated;

import static com.google.common.base.Preconditions.checkArgument;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;
import picante.mechanics.StateVector;

class NonInertialAberratedStateVectorFunction implements AberratedStateVectorFunction {

  private final AberratedStateVectorFunction inertialFunction;
  private final AberratedStateVectorFunction center;
  private final StateTransformFunction inertialToFrame;

  private final double ltSign;

  NonInertialAberratedStateVectorFunction(AberratedStateVectorFunction inertialFunction,
      AberratedStateVectorFunction center, StateTransformFunction inertialToFrame) {
    super();

    checkArgument(inertialFunction.getObserverID().equals(center.getObserverID()),
        "Inertially corrected vector function must have an observer: %s that"
            + " matches the inertially corrected function to the non-inertial "
            + "frame center function's observer: %s.",
        inertialFunction.getObserverID(), center.getObserverID());

    checkArgument(inertialFunction.getFrameID().isInertial());
    checkArgument(center.getFrameID().isInertial());

    checkArgument(inertialToFrame.getFromID().equals(inertialFunction.getFrameID()));

    this.inertialFunction = inertialFunction;
    this.center = center;
    this.inertialToFrame = inertialToFrame;

    this.ltSign = inertialFunction.getCorrection().isTransmission() ? 1.0 : -1.0;
  }

  @Override
  public EphemerisID getObserverID() {
    return inertialFunction.getObserverID();
  }

  @Override
  public EphemerisID getTargetID() {
    return inertialFunction.getTargetID();
  }

  @Override
  public FrameID getFrameID() {
    return inertialToFrame.getToID();
  }

  @Override
  public Coverage getCoverage() {
    /*
     * This is complicated to do properly, as it requires working out the light times between the
     * observer and frame center at the boundaries. For now just return the coverage of the
     * ephemeris function.
     */
    return inertialFunction.getCoverage();
  }

  @Override
  public AberrationCorrection getCorrection() {
    return inertialFunction.getCorrection();
  }

  @Override
  public VectorIJK getPosition(double time, VectorIJK buffer) {

    /*
     * Fetch the corrected (in the inertial frame) position.
     */
    inertialFunction.getPosition(time, buffer);

    /*
     * Compute the light time between the center and observer.
     */
    double ltime = center.getLightTime(time);

    /*
     * Look up the frame transformation at the appropriate time.
     */
    RotationMatrixIJK toFrame =
        inertialToFrame.getTransform(time + ltSign * ltime, new RotationMatrixIJK());

    return toFrame.mxv(buffer, buffer);
  }

  @Override
  public StateVector getState(double time, StateVector buffer) {

    buffer = buffer == null ? new StateVector() : buffer;

    /*
     * Fetch the corrected (in the inertial frame) state.
     */
    inertialFunction.getState(time, buffer);

    /*
     * Compute the light time and its derivative between the center and observer.
     * 
     * TODO: This is not as efficient as it might otherwise be. These two calls to obtain the light
     * time and its derivative result in multiple evaluations of the same vector functions under the
     * hood. Consider adding an internal method that returns both in a double[].
     */
    double ltime = center.getLightTime(time);
    double dLtime = center.getLightTimeDerivative(time);

    /*
     * Lookup the state transformation at the appropriate time.
     */
    StateTransform toFrame =
        inertialToFrame.getStateTransform(time + ltSign * ltime, new StateTransform());

    /*
     * There's a tricky bit here: since toFrame is evaluated at time
     * 
     * time + ltSign*ltime
     * 
     * toFrame is actually dependent on ltime. We need to account for this dependency in our
     * velocity transformation.
     * 
     * Let P and V be the target position and velocity respectively, and R, DR be the rotation and
     * rotation derivative corresponding to toFrame.
     * 
     * The state transformation we need to perform is not
     * 
     * R * V + DR * P
     * 
     * but rather
     * 
     * R * V + ( (1 + ltSign*dLtime) * DR ) * P
     * 
     * So we'll scale the derivative block of toFrame accordingly.
     */
    toFrame.getRotationDerivative().scale(1.0 + ltSign * dLtime);

    return toFrame.mxv(buffer, buffer);
  }

  @Override
  public double getLightTime(double time) {
    return inertialFunction.getLightTime(time);
  }

  @Override
  public double getLightTimeDerivative(double time) {
    return inertialFunction.getLightTimeDerivative(time);
  }

}
