package picante.mechanics.providers.aberrated;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkState;
import static picante.math.PicanteMath.asin;
import static picante.math.PicanteMath.max;
import static picante.math.PicanteMath.sqrt;
import picante.math.functions.DifferentiableVectorIJKFunction;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateVector;
import picante.mechanics.UnwritableStateVector;

class InertialStellarAberratedStateVectorFunction implements AberratedStateVectorFunction {

  /**
   * Let PHI be the (non-negative) rotation angle of the stellar aberration correction; then
   * SEPARATION_LIMIT is a limit on how close PHI may be to zero radians while stellar aberration
   * velocity is computed analytically. When sin(PHI) is lees than SEPARATION_LIMIT, the velocity
   * must be computed numerically.
   */
  private static final double SEPARATION_LIMIT = 1e-6;

  private static final double[] SIGNS = new double[] {-1.0, 1.0};

  private final InertialLightTimeStateVectorFunction ltFunction;
  private final DifferentiableVectorIJKFunction observerVelocity;
  private final AberrationCorrection correction;
  private final double derivativeDeltaT;

  InertialStellarAberratedStateVectorFunction(InertialLightTimeStateVectorFunction ltFunction,
      DifferentiableVectorIJKFunction obsAcceleration, AberrationCorrection correction,
      double derivativeDeltaT) {
    super();
    this.ltFunction = ltFunction;
    this.observerVelocity = obsAcceleration;
    /*
     * Only permit corrections that use stellar aberration.
     */
    checkArgument(correction.useStellarAberration());
    this.correction = correction;
    checkArgument(derivativeDeltaT > 0.0);
    this.derivativeDeltaT = derivativeDeltaT;
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

  @Override
  public double getLightTime(double time) {
    return ltFunction.getLightTime(time);
  }

  @Override
  public double getLightTimeDerivative(double time) {
    return ltFunction.getLightTimeDerivative(time);
  }

  /**
   * This code should directly inherit from
   * {@link InertialStellarAberratedPositionVectorFunction#getPosition(double, VectorIJK)} .
   * However, we have the actual velocity vector here so modify the code to utilize it instead of
   * the derivative approximation.
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
    VectorIJK velocity = observerVelocity.evaluate(time, new VectorIJK());
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

  /**
   * Compute the stellar aberration corrected state vector. The math used to derive this follows:
   * <p>
   * In the discussion below, the dot product of vectors X and Y is denoted by
   * 
   * <pre>
   *    <X,Y>
   * </pre>
   * 
   * The speed of light is denoted by the lower case letter "c." BTW, variable names used here are
   * case-sensitive: upper case "C" represents a different quantity which is unrelated to the speed
   * of light.
   * 
   * Variable names ending in "HAT" denote unit vectors. Variable names starting with "D" denote
   * derivatives with respect to time.
   * 
   * We'll compute the correction SCORR and its derivative with respect to time DSCORR for the
   * reception case. In the transmission case, we perform the same computation with the negatives of
   * the observer velocity and acceleration.
   * 
   * In the code below, we'll store the position and velocity portions of the input observer-target
   * state STARG in the variables PTARG and VTARG, respectively.
   * 
   * Let VP be the component of VOBS orthogonal to PTARG. VP is defined as
   * 
   * <pre>
   *     VOBS - < VOBS, RHAT > RHAT                                 (1)
   * </pre>
   * 
   * where RHAT is the unit vector
   * 
   * <pre>
   *     PTARG/||PTARG||
   * </pre>
   * 
   * Then
   * 
   * <pre>
   *    ||VP||/c                                                    (2)
   * </pre>
   * 
   * is the magnitude of
   * 
   * <pre>
   *    s = sin( phi )                                              (3)
   * </pre>
   * 
   * where phi is the stellar aberration correction angle. We'll need the derivative with respect to
   * time of (2).
   * 
   * Differentiating (1) with respect to time yields the velocity DVP, where, letting
   * 
   * <pre>
   *    DRHAT  =  d(RHAT) / dt 
   *    VPHAT  =  VP      / ||VP||
   *    DVPMAG =  d( ||VP|| ) / dt
   * </pre>
   * 
   * we have
   * 
   * <pre>
   *    DVP = d(VP)/dt 
   * 
   *        = ACCOBS - (  ( <VOBS,DRHAT> + <ACCOBS, RHAT> )*RHAT
   *                    +   <VOBS,RHAT>  * DRHAT                 )  (4)
   * </pre>
   * 
   * and
   * 
   * <pre>
   *    DVPMAG = < DVP, VPHAT >                                     (5)
   * </pre>
   * 
   * Now we can find the derivative with respect to time of the stellar aberration angle phi:
   * 
   * <pre>
   *    ds/dt = d(sin(phi))/dt = d(phi)/dt * cos(phi)               (6)
   * </pre>
   * 
   * Using (2) and (5), we have for positive phi,
   * 
   * <pre>
   *    ds/dt = (1/c)*DVPMAG = (1/c)*<DVP, VPHAT>                   (7)
   * </pre>
   * 
   * Then for positive phi
   * 
   * <pre>
   *    d(phi)/dt = (1/cos(phi)) * (1/c) * <DVP, VPHAT>             (8)
   * </pre>
   * 
   * Equation (8) is well-defined as along as VP is non-zero: if VP is the zero vector, VPHAT is
   * undefined. We'll treat the singular and near-singular cases separately.
   * 
   * The aberration correction itself is a rotation by angle phi from RHAT towards VP, so the
   * corrected vector is
   * 
   * <pre>
   *    ( sin(phi)*VPHAT + cos(phi)*RHAT ) * ||PTARG||
   * </pre>
   * 
   * and we can express the offset of the corrected vector from PTARG, which is the output SCORR, as
   * 
   * <pre>
   *    SCORR = 
   * 
   *    ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * ||PTARG||          (9)
   * </pre>
   * 
   * Let DPTMAG be defined as
   * 
   * <pre>
   *    DPTMAG  =  d ( ||PTARG|| ) / dt                            (10)
   * </pre>
   * 
   * Then the derivative with respect to time of SCORR is
   * 
   * <pre>
   *    DSCORR =
   * 
   *         (      sin(phi)*DVPHAT  
   * 
   *            +   cos(phi)*d(phi)/dt * VPHAT
   * 
   *            +  (cos(phi) - 1) * DRHAT
   * 
   *            +  ( -sin(phi)*d(phi)/dt ) * RHAT   ) * ||PTARG||
   * 
   *       + ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * DPTMAG       (11)
   * </pre>
   * 
   * </p>
   * {@inheritDoc}
   */
  StateVector computeCorrection(UnwritableVectorIJK accobs, UnwritableVectorIJK velobs,
      UnwritableStateVector starg, StateVector buffer) {

    /*
     * Follow the SPICE FORTRAN source conventions here, as this algorithm is sufficiently
     * complicated it will be difficult to trace otherwise.
     * 
     * Compute RHAT, VHAT, VP, DPTMAG. RHAT and VHAT reside in the newly created bufferHat state
     * vector:
     */
    VectorIJK lcvobs = new VectorIJK(velobs);
    VectorIJK lcacc = new VectorIJK(accobs);

    if (correction.isTransmission()) {
      lcvobs.negate();
      lcacc.negate();
    }

    buffer.setTo(starg);
    VectorIJK ptarg = buffer.getPosition();
    VectorIJK vtarg = buffer.getVelocity();

    StateVector srHat = buffer.createUnitized();
    VectorIJK rhat = srHat.getPosition();
    VectorIJK drhat = srHat.getVelocity();

    VectorIJK vp = VectorIJK.planeProject(lcvobs, rhat);

    double dptmag = buffer.getVelocity().getDot(rhat);

    /*
     * Compute sin(phi) and cos(phi). Note that phi is always close to zero for realistic inputs
     * (||VOBS|| << CLIGHT). So the cosine term should be positive.
     */
    double s = vp.getLength() / AberrationCorrection.SPEED_OF_LIGHT;
    double c = sqrt(max(0.0, 1 - s * s));

    /*
     * Compute the unit vector VPHAT and the stellar aberration correction.
     */
    VectorIJK vpHat = new VectorIJK(vp);
    if (vp.equals(VectorIJK.ZERO)) {
      vpHat.setTo(VectorIJK.ZERO);
    } else {
      vpHat.unitize();
    }

    /*
     * Now utilize equation (9) above in the javadoc to obtain the stellar aberration correction.
     */
    double ptgmag = ptarg.getLength();

    VectorIJK scorr = VectorIJK.combine(ptgmag * s, vpHat, ptgmag * (c - 1.0), rhat);

    /*
     * Now use sine as an estimate of PHI to decide if we're going to differentiate the stellar
     * aberration correction analytically or numerically.
     * 
     * Note that sine is non-negative by construction, so no need for |sine|
     */
    if (s >= SEPARATION_LIMIT) {

      return analyticalDerivative(lcacc, lcvobs, rhat, drhat, vp, vpHat, s, c, ptgmag, dptmag,
          scorr, buffer);

    }

    return numericalDerivative(derivativeDeltaT, SIGNS, lcacc, lcvobs, ptarg, vtarg, drhat, vp,
        vpHat, scorr, buffer);
  }

  static StateVector analyticalDerivative(VectorIJK lcacc, VectorIJK lcvobs, VectorIJK rhat,
      VectorIJK drhat, VectorIJK vp, VectorIJK vpHat, double s, double c, double ptgmag,
      double dptmag, VectorIJK scorr, StateVector buffer) {

    /*
     * This is the analytic case. Compute DVP--the derivative of VP with respect to time. Recall
     * equation (4) from the javadoc above:
     */
    VectorIJK dvp = VectorIJK.combine(1.0, lcacc, -lcvobs.getDot(drhat) - lcacc.getDot(rhat), rhat,
        -lcvobs.getDot(rhat), drhat);

    /*
     * Assemble the state vector for vp. Unitize it.
     */
    StateVector sHat = new StateVector(vp, dvp);
    sHat.unitize();
    VectorIJK dvphat = sHat.getVelocity();

    /*
     * Compute DVPHAT, the derivative of VPHAT.
     */
    double dphi = (1.0 / (c * AberrationCorrection.SPEED_OF_LIGHT)) * dvp.getDot(vpHat);

    VectorIJK term1 = VectorIJK.combine(s, dvphat, c * dphi, vpHat);
    VectorIJK term2 = VectorIJK.combine(c - 1.0, drhat, (-s) * dphi, rhat);
    VectorIJK term3 = VectorIJK.add(term1, term2);

    VectorIJK dscorr =
        VectorIJK.combine(ptgmag, term3, dptmag * s, vpHat, dptmag * (c - 1.0), rhat);

    buffer.setPosition(scorr);
    buffer.setVelocity(dscorr);

    return buffer;

  }

  static StateVector numericalDerivative(double derivativeDeltaT, double[] SIGNS, VectorIJK lcacc,
      VectorIJK lcvobs, VectorIJK ptarg, VectorIJK vtarg, VectorIJK rhat, VectorIJK vp,
      VectorIJK vpHat, VectorIJK scorr, StateVector buffer) {
    VectorIJK[] saoff = new VectorIJK[SIGNS.length];

    for (int i = 0; i < SIGNS.length; i++) {

      /*
       * Estimate the observer's velocity relative to the solar system barycenter at the current
       * epoch. We use the local copies of the input velocity and acceleration to make a linear
       * estimate.
       */
      VectorIJK evobs = VectorIJK.combine(1.0, lcvobs, SIGNS[i] * derivativeDeltaT, lcacc);

      /*
       * Estimate the observer-target vector. We use the observer target state velocity to make a
       * linear estimate.
       */
      VectorIJK eptarg = VectorIJK.combine(1.0, ptarg, SIGNS[i] * derivativeDeltaT, vtarg);

      /*
       * Let rhat be the unit observer-target position. Compute the component of the observer's
       * velocity that is perpendicular to the target position; call this vector vp. Also compute
       * the unit vector in the direction of vp.
       */
      rhat.setToUnitized(eptarg);
      VectorIJK.planeProject(evobs, rhat, vp);

      if (vp.equals(VectorIJK.ZERO)) {
        vp.setTo(VectorIJK.ZERO);
      } else {
        vpHat.setToUnitized(vp);
      }

      /*
       * Compute the sine and cosine of the correction angle.
       */
      double s = vp.getLength() / AberrationCorrection.SPEED_OF_LIGHT;
      double c = sqrt(max(0.0, 1.0 - s * s));

      /*
       * Compute the offset vector of the correction.
       */
      double ptgmag = eptarg.getLength();

      saoff[i] = VectorIJK.combine(ptgmag * s, vpHat, ptgmag * (c - 1.0), rhat);

    }

    /*
     * Estimate the derivative.
     */
    VectorIJK dscorr =
        VectorIJK.combine(0.5 / derivativeDeltaT, saoff[1], -0.5 / derivativeDeltaT, saoff[0]);

    buffer.setPosition(scorr);
    buffer.setVelocity(dscorr);

    return buffer;

  }

  @Override
  public StateVector getState(double time, StateVector buffer) {

    buffer = buffer == null ? new StateVector() : buffer;

    /*
     * Lookup the light time corrected state vector of interest.
     */
    ltFunction.getState(time, buffer);

    /*
     * Compute the observer's acceleration and velocity.
     */
    VectorIJK velocity = observerVelocity.evaluate(time, new VectorIJK());
    VectorIJK acceleration = observerVelocity.differentiate(time, new VectorIJK());

    /*
     * Estimate the stellar aberration correction.
     */
    StateVector correction = computeCorrection(acceleration, velocity, buffer, new StateVector());

    /*
     * Adding the stellar aberration correction to the light time corrected target position yields
     * the position corrected for both light time and stellar aberration.
     */
    StateVector.add(buffer, correction, buffer);

    return buffer;
  }

}
