package picante.spice.kernel.spk;

import picante.math.functions.ChebyshevPolynomial;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;

/**
 * Data contents of a type 3 record. Each record consists of six Chebyshev polynomials and a set of
 * coefficients for mapping between the supported time range and the standard Chebyshev evaluation
 * interval [-1,1].
 */
public class SPKType3Record {

  /**
   * The polynomial used to interpolate the x component of the position
   */
  private final ChebyshevPolynomial xPolynomial;

  /**
   * The polynomial used to interpolate the y component of the position
   */
  private final ChebyshevPolynomial yPolynomial;

  /**
   * The polynomial used to interpolate the z component of the position
   */
  private final ChebyshevPolynomial zPolynomial;

  /**
   * The polynomial used to interpolate the x component of the velocity
   */
  private final ChebyshevPolynomial dxPolynomial;

  /**
   * The polynomial used to interpolate the y component of the velocity
   */
  private final ChebyshevPolynomial dyPolynomial;

  /**
   * The polynomial used to interpolate the z component of the velocity
   */
  private final ChebyshevPolynomial dzPolynomial;

  /**
   * Creates a default, empty type 3 record.
   */
  public SPKType3Record() {
    this(2);
  }

  /**
   * Creates an empty type 3 record with storage space preallocated for a polynomial of the supplied
   * degree.
   * 
   * @param degree expected degree of the Chebyshev polynomial
   */
  public SPKType3Record(int degree) {
    double[] tmp = new double[(degree > 0) ? degree + 1 : 2];
    this.xPolynomial = new ChebyshevPolynomial(degree, tmp, tmp);
    this.yPolynomial = new ChebyshevPolynomial(degree, tmp, tmp);
    this.zPolynomial = new ChebyshevPolynomial(degree, tmp, tmp);
    this.dxPolynomial = new ChebyshevPolynomial(degree, tmp, tmp);
    this.dyPolynomial = new ChebyshevPolynomial(degree, tmp, tmp);
    this.dzPolynomial = new ChebyshevPolynomial(degree, tmp, tmp);
  }

  /**
   * Evaluates the polynomials stored in the record to provide the position at the requested time.
   * While extrapolation is allowed by the API, it is strongly discouraged.
   * 
   * @param time the ephemeris time of interest (TDB seconds past J2000)
   * @param buffer the buffer to populate with the results.
   * 
   * @return a reference to buffer for convenience
   */
  public VectorIJK evaluate(double time, VectorIJK buffer) {
    buffer.setI(xPolynomial.evaluate(time));
    buffer.setJ(yPolynomial.evaluate(time));
    buffer.setK(zPolynomial.evaluate(time));
    return buffer;
  }

  /**
   * Evaluates the polynomials stored in the record to provide the state at the requested time.
   * While extrapolation is allowed by the API, it is strongly discouraged.
   * 
   * @param time the ephemeris time of interest (TDB seconds past J2000)
   * @param buffer the buffer to populate with results
   * 
   * @return a reference to buffer for convenience
   */
  public StateVector evaluate(double time, StateVector buffer) {

    VectorIJK p = buffer.getPosition();
    VectorIJK v = buffer.getVelocity();

    p.setI(xPolynomial.evaluate(time));
    p.setJ(yPolynomial.evaluate(time));
    p.setK(zPolynomial.evaluate(time));

    v.setI(dxPolynomial.evaluate(time));
    v.setJ(dyPolynomial.evaluate(time));
    v.setK(dzPolynomial.evaluate(time));

    return buffer;

  }

  /**
   * Sets the coefficients of the underlying Chebyshev polynomials in the record.
   * 
   * @param degree the degree of the three polynomials
   * @param xCoeffs an array containing the degree+1 coefficients for the x polynomial
   * @param xCoeffsOffset the offset into the xCoeffs array at which to start extracting the
   *        degree+1 coefficients
   * @param yCoeffs an array containing the degree+1 coefficients for the x polynomial
   * @param yCoeffsOffset the offset into the xCoeffs array at which to start extracting the
   *        degree+1 coefficients
   * @param zCoeffs an array containing the degree+1 coefficients for the x polynomial
   * @param zCoeffsOffset the offset into the xCoeffs array at which to start extracting the
   *        degree+1 coefficients
   * @param dxCoeffs an array containing the degree+1 coefficients for the x derivative polynomial
   * @param dxCoeffsOffset the offset into the dxCoeffs array at which to start extracting the
   *        degree+1 coefficients
   * @param dyCoeffs an array containing the degree+1 coefficients for the y derivative polynomial
   * @param dyCoeffsOffset the offset into the dyCoeffs array at which to start extracting the
   *        degree+1 coefficients
   * @param dzCoeffs an array containing the degree+1 coefficients for the z derivative polynomial
   * @param dzCoeffsOffset the offset into the dzCoeffs array at which to start extracting the
   *        degree+1 coefficients
   * @param x2s the 2 coefficients that provide the mapping between the supported time range and the
   *        [-1,1] interval
   * @param x2sOffset the offset into the x2s array at which to start extracting the 2 coefficients
   *        for the x2s mapping
   */
  public void setCoefficients(int degree, double[] xCoeffs, int xCoeffsOffset, double[] yCoeffs,
      int yCoeffsOffset, double[] zCoeffs, int zCoeffsOffset, double[] dxCoeffs, int dxCoeffsOffset,
      double[] dyCoeffs, int dyCoeffsOffset, double[] dzCoeffs, int dzCoeffsOffset, double[] x2s,
      int x2sOffset) {
    xPolynomial.setCoefficients(degree, xCoeffs, xCoeffsOffset, x2s, x2sOffset);
    yPolynomial.setCoefficients(degree, yCoeffs, yCoeffsOffset, x2s, x2sOffset);
    zPolynomial.setCoefficients(degree, zCoeffs, zCoeffsOffset, x2s, x2sOffset);
    dxPolynomial.setCoefficients(degree, dxCoeffs, dxCoeffsOffset, x2s, x2sOffset);
    dyPolynomial.setCoefficients(degree, dyCoeffs, dyCoeffsOffset, x2s, x2sOffset);
    dzPolynomial.setCoefficients(degree, dzCoeffs, dzCoeffsOffset, x2s, x2sOffset);
  }

  public ChebyshevPolynomial getXPolynomial(ChebyshevPolynomial buffer) {
    buffer.setCoefficients(xPolynomial);
    return buffer;
  }

  public ChebyshevPolynomial getYPolynomial(ChebyshevPolynomial buffer) {
    buffer.setCoefficients(yPolynomial);
    return buffer;
  }

  public ChebyshevPolynomial getZPolynomial(ChebyshevPolynomial buffer) {
    buffer.setCoefficients(zPolynomial);
    return buffer;
  }

  public ChebyshevPolynomial getDXPolynomial(ChebyshevPolynomial buffer) {
    buffer.setCoefficients(dxPolynomial);
    return buffer;
  }

  public ChebyshevPolynomial getDYPolynomial(ChebyshevPolynomial buffer) {
    buffer.setCoefficients(dyPolynomial);
    return buffer;
  }

  public ChebyshevPolynomial getDZPolynomial(ChebyshevPolynomial buffer) {
    buffer.setCoefficients(dzPolynomial);
    return buffer;
  }

}
