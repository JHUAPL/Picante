package picante.spice.kernel.pck;

import picante.math.functions.ChebyshevPolynomial;
import picante.mechanics.rotations.DifferentiatedEulerAngles;
import picante.mechanics.rotations.EulerAngles;
import picante.spice.kernel.utilities.ChebyshevTriplet;

public class PCKType2Record implements ChebyshevTriplet {

  private static final double TWOPI = 2.0 * Math.PI;

  private final ChebyshevPolynomial aPolynomial;
  private final ChebyshevPolynomial bPolynomial;
  private final ChebyshevPolynomial cPolynomial;

  private final double[] values = new double[2];

  public PCKType2Record() {
    this(2);
  }

  public PCKType2Record(int degree) {
    double[] tmp = new double[(degree > 0) ? degree + 1 : 2];
    this.aPolynomial = new ChebyshevPolynomial(degree, tmp, tmp);
    this.bPolynomial = new ChebyshevPolynomial(degree, tmp, tmp);
    this.cPolynomial = new ChebyshevPolynomial(degree, tmp, tmp);
  }

  public EulerAngles.KIK evaluate(double time, EulerAngles.KIK buffer) {
    buffer.set(cPolynomial.evaluate(time) % TWOPI, bPolynomial.evaluate(time),
        aPolynomial.evaluate(time));
    return buffer;
  }

  public DifferentiatedEulerAngles.KIK evaluate(double time, DifferentiatedEulerAngles.KIK buffer) {
    cPolynomial.evaluate(time, values);
    buffer.setLeftAngle(values[0] % TWOPI);
    buffer.setLeftAngleDerivative(values[1]);
    bPolynomial.evaluate(time, values);
    buffer.setCenterAngle(values[0]);
    buffer.setCenterAngleDerivative(values[1]);
    aPolynomial.evaluate(time, values);
    buffer.setRightAngle(values[0]);
    buffer.setRightAngleDerivative(values[1]);
    return buffer;
  }

  /**
   * {@inheritDoc}
   * 
   * <p>
   * The first, second, and third Chebyshev polynomials define the first, second and third Euler
   * angles and their derivatives in the {@link EulerAngles.KIK} decomposition.
   * </p>
   */
  @Override
  public void setCoefficients(int degree, double[] aCoeffs, int aCoeffsOffset, double[] bCoeffs,
      int bCoeffsOffset, double[] cCoeffs, int cCoeffsOffset, double[] x2s, int x2sOffset) {
    aPolynomial.setCoefficients(degree, aCoeffs, aCoeffsOffset, x2s, x2sOffset);
    bPolynomial.setCoefficients(degree, bCoeffs, bCoeffsOffset, x2s, x2sOffset);
    cPolynomial.setCoefficients(degree, cCoeffs, cCoeffsOffset, x2s, x2sOffset);
  }

  public ChebyshevPolynomial getAPolynomial(ChebyshevPolynomial buffer) {
    buffer.setCoefficients(aPolynomial);
    return buffer;
  }

  public ChebyshevPolynomial getBPolynomial(ChebyshevPolynomial buffer) {
    buffer.setCoefficients(bPolynomial);
    return buffer;
  }

  public ChebyshevPolynomial getCPolynomial(ChebyshevPolynomial buffer) {
    buffer.setCoefficients(cPolynomial);
    return buffer;
  }

}
