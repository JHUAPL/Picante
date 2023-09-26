package picante.mechanics.providers.aberrated;

import static com.google.common.base.Preconditions.checkArgument;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.Coverages;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.PositionVectorFunction;

class InertialLightTimePositionVectorFunction implements AberratedPositionVectorFunction {

  private final PositionVectorFunction observer;
  private final PositionVectorFunction target;

  private final Coverage coverage;
  private final AberrationCorrection correction;
  final int numberOfIterations;
  final double lightTimeSign;

  InertialLightTimePositionVectorFunction(PositionVectorFunction observer,
      PositionVectorFunction target, AberrationCorrection correction, int numberOfIterations) {
    super();

    checkArgument(observer.getObserverID().equals(target.getObserverID()),
        "Target and observer functions must have equivalent observerID. Observer=%s. Target=%s",
        observer.getObserverID(), target.getObserverID());

    checkArgument(observer.getFrameID().equals(target.getFrameID()),
        "Target and observer frameID must match. Observer=%s. Target=%s", observer.getFrameID(),
        target.getFrameID());

    checkArgument(observer.getFrameID().isInertial(),
        "Target and observer reference frame must be inertial.");

    checkArgument(correction == AberrationCorrection.LT || correction == AberrationCorrection.XLT,
        "Requested correction must be either %s or %s, was %s", AberrationCorrection.LT,
        AberrationCorrection.XLT, correction);

    checkArgument(numberOfIterations > 0, "Number of iterations must exceed 0, was %s",
        numberOfIterations);

    this.observer = observer;
    this.target = target;
    this.coverage = Coverages.intersect(target.getCoverage(), observer.getCoverage());
    this.correction = correction;
    this.numberOfIterations = numberOfIterations;
    /*
     * Set the sign of the light time correction. If it's transmission we will be adding time to the
     * target ephemeris query to locate where it will be when photons arrive.
     */
    this.lightTimeSign = correction.isTransmission() ? 1 : -1;
  }

  @Override
  public EphemerisID getObserverID() {
    return observer.getTargetID();
  }

  @Override
  public EphemerisID getTargetID() {
    return target.getTargetID();
  }

  @Override
  public FrameID getFrameID() {
    return observer.getFrameID();
  }

  @Override
  public Coverage getCoverage() {
    return coverage;
  }

  @Override
  public AberrationCorrection getCorrection() {
    return correction;
  }

  @Override
  public VectorIJK getPosition(double time, VectorIJK buffer) {

    buffer = buffer == null ? new VectorIJK() : buffer;

    computeLightTimeCorrectedState(time, buffer);
    return buffer;
  }

  @Override
  public double getLightTime(double time) {
    return computeLightTimeCorrectedState(time, new VectorIJK());
  }

  /**
   * Internal method that consolidates the computation of the light time and the light time
   * corrected state.
   * 
   * @param time
   * @param buffer
   * 
   * @return
   */
  private double computeLightTimeCorrectedState(double time, VectorIJK buffer) {

    /*
     * Look up the position of the observer relative to the inertial reference. This vector remains
     * a constant during the computation, as time is specified at the observer.
     */
    VectorIJK observerPosition = observer.getPosition(time, new VectorIJK());

    /*
     * Fetch the position of the target relative to the inertial reference. Subtract the observer's
     * position from it.
     */
    target.getPosition(time, buffer);
    VectorIJK.subtract(buffer, observerPosition, buffer);

    double lightTime = buffer.getLength() / AberrationCorrection.SPEED_OF_LIGHT;

    /*
     * We've done one iteration already, perform any additional iterations requested.
     */
    for (int i = 0; i < numberOfIterations; i++) {
      target.getPosition(time + lightTimeSign * lightTime, buffer);
      VectorIJK.subtract(buffer, observerPosition, buffer);
      lightTime = buffer.getLength() / AberrationCorrection.SPEED_OF_LIGHT;
    }

    return lightTime;

  }

}
