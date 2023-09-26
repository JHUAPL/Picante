package picante.mechanics.providers.aberrated;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;
import static com.google.common.base.Preconditions.checkState;
import static picante.math.PicanteMath.asin;
import picante.math.functions.DifferentiableVectorIJKFunction;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;

class InertialStellarAberratedPositionVectorFunction implements AberratedPositionVectorFunction {

  private final InertialLightTimePositionVectorFunction ltFunction;
  private final DifferentiableVectorIJKFunction observerPosition;
  private final AberrationCorrection correction;

  InertialStellarAberratedPositionVectorFunction(InertialLightTimePositionVectorFunction ltFunction,
      DifferentiableVectorIJKFunction obsVelocity, AberrationCorrection correction) {
    super();
    this.ltFunction = checkNotNull(ltFunction);
    this.observerPosition = checkNotNull(obsVelocity);
    /*
     * Only permit corrections that use stellar aberration.
     */
    checkArgument(correction.useStellarAberration());
    this.correction = correction;
  }

  @Override
  public EphemerisID getObserverID() {
    return ltFunction.getObserverID();
  }

  @Override
  public EphemerisID getTargetID() {
    return ltFunction.getTargetID();
  }

  @Override
  public FrameID getFrameID() {
    return ltFunction.getFrameID();
  }

  @Override
  public Coverage getCoverage() {
    return ltFunction.getCoverage();
  }

  @Override
  public AberrationCorrection getCorrection() {
    return correction;
  }

  /**
   * Compute the stellar aberration corrected position.
   * <p>
   * Let r be the vector from the observer to the object, and v be - the velocity of the observer
   * with respect to the inertial reference point. Let w be the angle between them. The aberration
   * angle phi is given by
   * 
   * <pre>
   *      sin(phi) = v sin(w) / c
   * </pre>
   * 
   * Let h be the vector given by the cross product
   * 
   * <pre>
   *       h = r X v
   * </pre>
   * 
   * Rotate r by phi radians about h to obtain the apparent position of the object.
   * </p>
   * {@inheritDoc}
   */
  @Override
  public VectorIJK getPosition(double time, VectorIJK buffer) {

    buffer = buffer == null ? new VectorIJK() : buffer;

    /*
     * Compute the unit vector in the direction of the uncorrected vector.
     */
    ltFunction.getPosition(time, buffer);
    VectorIJK bufferHat = buffer.createUnitized();

    /*
     * Compute the observer's velocity, and scale it by the speed of light.
     */
    VectorIJK velocity = observerPosition.differentiate(time, new VectorIJK());
    velocity.scale(1.0 / AberrationCorrection.SPEED_OF_LIGHT);

    /*
     * Negate the velocity, if the correction requested is the transmission case. This will produce
     * the correct result.
     */
    if (correction.isTransmission()) {
      velocity.negate();
    }

    /*
     * Check that the observer's velocity does not exceed the speed of light. This doesn't need to
     * be extremely accurate, since things generally aren't flitting around at or near that speed.
     */
    checkState(velocity.getDot(velocity) < 1.0, "Velocity of observer exceeds the speed of light.");

    /*
     * Compute the cross product of the unit vector with the scaled observer's velocity.
     */
    VectorIJK.cross(bufferHat, velocity, velocity);

    double sinPhi = velocity.getLength();

    /*
     * If the look direction lies along the velocity vector (sinPhi = 0) there's no correction to
     * apply.
     */
    if (sinPhi != 0.0) {
      double phi = asin(sinPhi);
      VectorIJK.rotate(buffer, velocity, phi, buffer);
    }

    return buffer;
  }

  @Override
  public double getLightTime(double time) {
    return ltFunction.getLightTime(time);
  }

}
