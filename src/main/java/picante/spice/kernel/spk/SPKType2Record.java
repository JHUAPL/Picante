package picante.spice.kernel.spk;

import picante.math.functions.ChebyshevPolynomial;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;
import picante.spice.kernel.utilities.ChebyshevTriplet;

/**
 * Data contents of a type 2 record. Each record consists of three Chebyshev polynomials and a set
 * of coefficients for mapping between the supported time range and the standard interval [-1,1].
 */
public class SPKType2Record implements ChebyshevTriplet {

  /**
   * The polynomial used to interpolate the x component of the state
   */
  private final ChebyshevPolynomial xPolynomial;

  /**
   * The polynomial used to interpolate the y component of the state
   */
  private final ChebyshevPolynomial yPolynomial;

  /**
   * The polynomial used to interpolate the z component of the state
   */
  private final ChebyshevPolynomial zPolynomial;

  /**
   * Buffer used to store intermediate computation results.
   */
  private final double[] values = new double[2];

  /**
   * Creates a default, empty type 2 record.
   */
  public SPKType2Record() {
    this(2);
  }

  /**
   * Creates an empty type 2 record with storage space preallocated for a polynomial of the supplied
   * degree.
   * 
   * @param degree expected degree of the Chebyshev polynomial
   */
  public SPKType2Record(int degree) {
    double[] tmp = new double[(degree > 0) ? degree + 1 : 2];
    this.xPolynomial = new ChebyshevPolynomial(degree, tmp, tmp);
    this.yPolynomial = new ChebyshevPolynomial(degree, tmp, tmp);
    this.zPolynomial = new ChebyshevPolynomial(degree, tmp, tmp);
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

    xPolynomial.evaluate(time, values);
    p.setI(values[0]);
    v.setI(values[1]);

    yPolynomial.evaluate(time, values);
    p.setJ(values[0]);
    v.setJ(values[1]);

    zPolynomial.evaluate(time, values);
    p.setK(values[0]);
    v.setK(values[1]);

    return buffer;

  }

  /**
   * {@inheritDoc}
   * 
   * <p>
   * The first, second and third Chebyshev polynomials in this triplet define the x, y, and z
   * components of the state vector and its derivative.
   * </p>
   */
  @Override
  public void setCoefficients(int degree, double[] xCoeffs, int xCoeffsOffset, double[] yCoeffs,
      int yCoeffsOffset, double[] zCoeffs, int zCoeffsOffset, double[] x2s, int x2sOffset) {
    xPolynomial.setCoefficients(degree, xCoeffs, xCoeffsOffset, x2s, x2sOffset);
    yPolynomial.setCoefficients(degree, yCoeffs, yCoeffsOffset, x2s, x2sOffset);
    zPolynomial.setCoefficients(degree, zCoeffs, zCoeffsOffset, x2s, x2sOffset);
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

}
