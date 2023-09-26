package picante.spice.kernel.tk.pck;

import com.google.common.base.Preconditions;

/**
 * Extension of the PCKFrameFunction that includes nutation and precession.
 */
public class PCKWithNutationFrameFunction extends PCKFrameFunction {

  private final double[][] nutPrecAngles;
  private final double[] nutPrecRA;
  private final double[] nutPrecDEC;
  private final double[] nutPrecW;

  private final double[] sinTheta;
  private final double[] cosTheta;
  private final double[] dSinTheta;
  private final double[] dCosTheta;

  /**
   * Creates a PCK frame function with nutation and precession.
   * 
   * @param timeConverter the time converter which converts TDB seconds past J2000 into the
   *        appropriate time base for the polynomial evaluation
   * @param referenceCode the SPICE integer code for the reference frame from which vectors are
   *        transformed
   * @param bodyCode the SPICE integer code for the body to whose fixed frame vectors are
   *        transformed
   * @param poleRA the quadratic coefficients of the RA component angle
   * @param poleDEC the quadratic coefficients of the DEC component angle
   * @param pm the quadratic coefficients of the PM component angle
   * @param nutPrecAngles a 2D array of length [2][n] where the first index captures the constant
   *        and linear terms of the nutation angle[i] evaluation
   * @param nutPrecRA an array of length no more than n containing the nutation and precession
   *        coefficients for the RA component
   * @param nutPrecDEC an array of length no more than n containing the nutation and precession
   *        coefficients for the DEC component
   * @param nutPrecW an array of length no more than n containing the nutation and precession
   *        coefficients for the W component
   */
  public PCKWithNutationFrameFunction(TimeConverter timeConverter, int referenceCode, int bodyCode,
      double[] poleRA, double[] poleDEC, double[] pm, double[][] nutPrecAngles, double[] nutPrecRA,
      double[] nutPrecDEC, double[] nutPrecW) {
    super(timeConverter, referenceCode, bodyCode, poleRA, poleDEC, pm);

    /*
     * Verify that the supplied nutPrecAngles 2D array has consistent lengths.
     */
    Preconditions.checkArgument(nutPrecAngles[0].length == nutPrecAngles[1].length,
        "Invalid PCK nutation and precession angles.");

    this.nutPrecAngles = new double[2][nutPrecAngles[0].length];
    System.arraycopy(nutPrecAngles[0], 0, this.nutPrecAngles[0], 0, this.nutPrecAngles[0].length);
    System.arraycopy(nutPrecAngles[1], 0, this.nutPrecAngles[1], 0, this.nutPrecAngles[0].length);

    this.nutPrecRA = new double[nutPrecRA.length];
    System.arraycopy(nutPrecRA, 0, this.nutPrecRA, 0, this.nutPrecRA.length);

    this.nutPrecDEC = new double[nutPrecDEC.length];
    System.arraycopy(nutPrecDEC, 0, this.nutPrecDEC, 0, this.nutPrecDEC.length);

    this.nutPrecW = new double[nutPrecW.length];
    System.arraycopy(nutPrecW, 0, this.nutPrecW, 0, this.nutPrecW.length);

    /*
     * nutPrecAngles[{0,1}] are supposed to be the same length, which should exceed or equal the
     * length of nutPrecRA, nutPrecDEC, nutPrecW. Setup the trigonometric cache arrays.
     */
    this.sinTheta = new double[nutPrecAngles[1].length];
    this.cosTheta = new double[nutPrecAngles[1].length];
    this.dSinTheta = new double[nutPrecAngles[1].length];
    this.dCosTheta = new double[nutPrecAngles[1].length];

  }

  @Override
  void prepareNutationAndLibration(double epoch) {
    for (int i = 0; i < nutPrecAngles[0].length; i++) {
      double theta = Math.toRadians(
          nutPrecAngles[0][i] + epoch / JULIAN_SECONDS_PER_CENTURY * nutPrecAngles[1][i]);
      sinTheta[i] = Math.sin(theta);
      cosTheta[i] = Math.cos(theta);
    }
  }

  @Override
  void prepareNutationAndLibrationWithDerivatives(double epoch) {
    for (int i = 0; i < nutPrecAngles[0].length; i++) {
      double theta = Math.toRadians(
          nutPrecAngles[0][i] + epoch / JULIAN_SECONDS_PER_CENTURY * nutPrecAngles[1][i]);
      double dTheta = Math.toRadians(nutPrecAngles[1][i] / JULIAN_SECONDS_PER_CENTURY);
      sinTheta[i] = Math.sin(theta);
      cosTheta[i] = Math.cos(theta);
      dSinTheta[i] = Math.cos(theta) * dTheta;
      dCosTheta[i] = -Math.sin(theta) * dTheta;
    }
  }

  @Override
  double computeRA(double epoch) {
    return super.computeRA(epoch) + evaluate(nutPrecRA, sinTheta);
  }

  @Override
  double computeDEC(double epoch) {
    return super.computeDEC(epoch) + evaluate(nutPrecDEC, cosTheta);
  }

  @Override
  double computeW(double epoch) {
    return super.computeW(epoch) + evaluate(nutPrecW, sinTheta);
  }

  @Override
  double computeRADerivative(double epoch) {
    return super.computeRADerivative(epoch) + evaluate(nutPrecRA, dSinTheta);
  }

  @Override
  double computeDECDerivative(double epoch) {
    return super.computeDECDerivative(epoch) + evaluate(nutPrecDEC, dCosTheta);
  }

  @Override
  double computeWDerivative(double epoch) {
    return super.computeWDerivative(epoch) + evaluate(nutPrecW, dSinTheta);
  }

  private double evaluate(double[] nutation, double[] trigTheta) {
    double result = 0;

    for (int i = 0; i < nutation.length; i++) {
      result += nutation[i] * trigTheta[i];
    }

    return result;
  }
}
