package picante.spice.kernel.tk.pck;

import static picante.units.FundamentalPhysicalConstants.HALFPI;
import static picante.units.FundamentalPhysicalConstants.SECONDS_PER_DAY;
import static picante.units.FundamentalPhysicalConstants.TWOPI;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.StateTransform;
import picante.mechanics.rotations.DifferentiatedEulerAngles;
import picante.mechanics.rotations.EulerAngles;

/**
 * Implementation of the basic text PCK body fixed frame definitions without any nutation or
 * precession.
 * <p>
 * Rotations transform vectors from the reference frame to the frame fixed to the body identified by
 * bodyCode.
 * </p>
 */
public class PCKFrameFunction {

  /**
   * The number of days in a Julian century.
   */
  final static double JULIAN_SECONDS_PER_CENTURY = 36525.0 * SECONDS_PER_DAY;

  /**
   * Handles conversion from TDB seconds past J2000 to whatever time base (usually seconds past some
   * other reference epoch in TDB) used to evaluate the polynomial coefficients.
   */
  private final TimeConverter timeConverter;

  /**
   * SPICE code for an inertial frame.
   */
  private final int referenceCode;

  /**
   * SPICE code for a celestial body.
   */
  private final int bodyCode;

  private final DifferentiatedEulerAngles.KIK euler = new DifferentiatedEulerAngles.KIK();

  private final double[] poleRA = new double[3];
  private final double[] poleDEC = new double[3];
  private final double[] pm = new double[3];

  /**
   * Creates a PCKFrameFunction, no checking is performed on the input arguments with regards to the
   * specifications indicated below.
   * 
   * @param timeConverter converts TDB seconds past J2000 to the time base associated with the
   *        polynomial's
   * @param referenceCode the SPICE integer code associated with the inertial reference frame
   * @param bodyCode the SPICE integer code identifying the body to which the frame definition is
   *        connected
   * @param poleRA an array of length 3 containing the constant, linear, and quadratic term in that
   *        order for the base RA angle
   * @param poleDEC an array of length 3 containing the constant, linear, and quadratic term in that
   *        order for the base DEC angle
   * @param pm an array of length 3 containing the constant, linear, and quadratic term in that
   *        order for the base PM angle
   */
  public PCKFrameFunction(TimeConverter timeConverter, int referenceCode, int bodyCode,
      double[] poleRA, double[] poleDEC, double[] pm) {
    this.timeConverter = timeConverter;
    this.referenceCode = referenceCode;
    this.bodyCode = bodyCode;

    System.arraycopy(poleRA, 0, this.poleRA, 0, this.poleRA.length);
    System.arraycopy(poleDEC, 0, this.poleDEC, 0, this.poleDEC.length);
    System.arraycopy(pm, 0, this.pm, 0, this.pm.length);
  }

  /**
   * Retrieves the SPICE integer code for the reference frame from which vectors are rotated
   * 
   * @return a SPICE inertial reference ID code
   */
  public int getReferenceCode() {
    return referenceCode;
  }

  /**
   * Retrieves the SPICE integer code for the body whose body-fixed frame vectors are rotated to
   * 
   * @return a SPICE body ID code
   */
  public int getBodyCode() {
    return bodyCode;
  }

  /**
   * Prepare any nutation and libration terms, if necessary.
   * <p>
   * This is simply a hook for the nutation sub-class to utilize to populate internal buffers for
   * its computations.
   * </p>
   * 
   * @param epoch the result of {@link #computeEpoch(double)} for the supplied time
   * 
   */
  void prepareNutationAndLibration(@SuppressWarnings("unused") double epoch) {}

  /**
   * Prepare any nutation and libration terms, along with their derivatives, if necessary.
   * <p>
   * This is simply a hook for the nutation sub-class to utilize to populate internal buffers for
   * its computations.
   * </p>
   * 
   * @param epoch the result of {@link #computeEpoch(double)} for the supplied time
   */
  void prepareNutationAndLibrationWithDerivatives(@SuppressWarnings("unused") double epoch) {}

  double computeEpoch(double time) {
    return timeConverter.computeEvaluationTime(time);
  }

  /**
   * Computes the right ascension angle.
   * 
   * @param epoch the count of Julian centuries past the reference epoch for the polynomial
   * 
   * @return the nominal right ascension angle in degrees.
   */
  double computeRA(double epoch) {
    return evaluateQuadratic(epoch / JULIAN_SECONDS_PER_CENTURY, poleRA);
  }

  double computeRADerivative(double epoch) {
    return evaluateQuadraticDerivative(epoch / JULIAN_SECONDS_PER_CENTURY, poleRA)
        / JULIAN_SECONDS_PER_CENTURY;
  }

  /**
   * Computes the declination angle.
   * 
   * @param epoch the count of Julian centuries past the reference epoch for the polynomial
   * 
   * @return the nominal declination angle in degrees
   */
  double computeDEC(double epoch) {
    return evaluateQuadratic(epoch / JULIAN_SECONDS_PER_CENTURY, poleDEC);
  }

  double computeDECDerivative(double epoch) {
    return evaluateQuadraticDerivative(epoch / JULIAN_SECONDS_PER_CENTURY, poleDEC)
        / JULIAN_SECONDS_PER_CENTURY;
  }

  /**
   * Computes the omega angle for the prime meridian location.
   * 
   * @param epoch the count of Julian days past the reference epoch for the polynomial.
   * 
   * @return the omega term in degrees
   */
  double computeW(double epoch) {
    return evaluateQuadratic(epoch / SECONDS_PER_DAY, pm);
  }

  double computeWDerivative(double epoch) {
    return evaluateQuadraticDerivative(epoch / SECONDS_PER_DAY, pm) / SECONDS_PER_DAY;
  }

  /**
   * Computes the transformation from the reference from to the body fixed frame.
   * 
   * @param time the TDB seconds past J2000 of interest
   * @param buffer a buffer to receive the resultant rotation
   * 
   * @return a reference to buffer for convenience
   */
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {

    double epoch = computeEpoch(time);
    prepareNutationAndLibration(epoch);

    double ra = computeRA(epoch);
    double dec = computeDEC(epoch);
    double w = computeW(epoch);

    /*
     * Convert ra, dec, and w to radians.
     */
    ra = Math.toRadians(ra);
    dec = Math.toRadians(dec);
    w = Math.toRadians(w);

    /*
     * Constrain ra, dec, and w to -2PI to 2PI.
     */
    ra = ra % TWOPI;
    dec = dec % TWOPI;
    w = w % TWOPI;

    /*
     * Make the appropriate adjustments for the proper decomposition.
     */
    ra += HALFPI;
    dec = HALFPI - dec;

    EulerAngles.KIK angles = euler.getRotation();

    angles.set(w, dec, ra);
    return angles.getRotation(buffer);
  }

  public StateTransform getStateTransform(double time, StateTransform buffer) {

    double epoch = computeEpoch(time);
    prepareNutationAndLibrationWithDerivatives(epoch);

    /*
     * Compute ra, dec, and w in radians along with their derivatives.
     */
    double ra = Math.toRadians(computeRA(time));
    double dec = Math.toRadians(computeDEC(epoch));
    double w = Math.toRadians(computeW(epoch));

    double dra = Math.toRadians(computeRADerivative(time));
    double ddec = Math.toRadians(computeDECDerivative(time));
    double dw = Math.toRadians(computeWDerivative(time));

    /*
     * For some reason TISBOD only applies the TWOPI wrapping to the W angle.
     */
    w = w % TWOPI;

    /*
     * Convert the angles to their Euler equivalents. Negate the derivative of declination due to
     * the negation in the conversion.
     */
    ra += HALFPI;
    dec = HALFPI - dec;

    ddec = -ddec;

    euler.set(w, dec, ra, dw, ddec, dra);
    return euler.getTransform(buffer);
  }

  /**
   * Evaluates a quadratic polynomial at the specified argument
   * 
   * @param t the argument of the polynomial
   * @param coeffs an array containing the constant, linear, and quadratic coefficients respectively
   * 
   * @return coeffs[0] + coeffs[1]*t + coeffs[2]*t*t
   */
  private double evaluateQuadratic(double t, double[] coeffs) {
    return coeffs[0] + t * (coeffs[1] + t * coeffs[2]);
  }

  /**
   * Evaluates the derivative of a quadratic polynomial at the specified argument
   * <p>
   * Note, if you opt to scale the time input, you will need to multiply the value computed here by
   * the scale factor to recover the actual derivative.
   * </p>
   * 
   * 
   * @param t the argument of the polynomial
   * 
   * @param coeffs an array containing the constant, linear, and quadratic coefficients respectively
   * 
   * @return coeffs[1] + 2.0 * t * coeffs[2]
   * 
   */
  private double evaluateQuadraticDerivative(double t, double[] coeffs) {
    return coeffs[1] + 2.0 * t * coeffs[2];
  }
}
