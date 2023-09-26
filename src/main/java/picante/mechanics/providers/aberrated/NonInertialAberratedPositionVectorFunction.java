package picante.mechanics.providers.aberrated;

import static com.google.common.base.Preconditions.checkArgument;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.FrameTransformFunction;

class NonInertialAberratedPositionVectorFunction implements AberratedPositionVectorFunction {

  private final AberratedPositionVectorFunction inertialFunction;
  private final AberratedPositionVectorFunction center;
  private final FrameTransformFunction inertialToFrame;

  private final double ltSign;

  NonInertialAberratedPositionVectorFunction(AberratedPositionVectorFunction inertialFunction,
      AberratedPositionVectorFunction center, FrameTransformFunction inertialToFrame) {
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
     * observer and the frame center at the boundaries. For now just return the coverage of the
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

    buffer = buffer == null ? new VectorIJK() : buffer;

    /*
     * Fetch the corrected (in the inertial frame) position.
     */
    inertialFunction.getPosition(time, buffer);

    /*
     * Compute the light time between the center and the observer. This is readily provided by the
     * center function.
     */
    double ltime = center.getLightTime(time);

    /*
     * Lookup the frame transformation at the appropriate time.
     */
    RotationMatrixIJK toFrame =
        inertialToFrame.getTransform(time + ltSign * ltime, new RotationMatrixIJK());

    return toFrame.mxv(buffer, buffer);
  }

  @Override
  public double getLightTime(double time) {
    return inertialFunction.getLightTime(time);
  }

}
