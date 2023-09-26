package picante.math.functions;

public class LagrangePolynomial implements DifferentiableUnivariateFunction {

  private double[] xvals;
  private double[] yvals;
  private int nPoints;

  private double[][] work;
  private final double[] tmp = new double[2];

  public LagrangePolynomial() {
    this.nPoints = 2;
    this.xvals = new double[nPoints];
    this.yvals = new double[nPoints];
    this.xvals[0] = 0.0;
    this.xvals[1] = 1.0;
    this.work = new double[2][nPoints];
  }

  public LagrangePolynomial(int nPoints, double[] xvals, double[] yvals) {
    this.nPoints = nPoints;
    this.xvals = new double[nPoints];
    this.yvals = new double[nPoints];
    System.arraycopy(xvals, 0, this.xvals, 0, nPoints);
    System.arraycopy(yvals, 0, this.yvals, 0, nPoints);

    this.work = new double[2][this.nPoints];
  }

  public int getNumberOfCoefficients() {
    return nPoints;
  }

  public int getDegree() {
    return nPoints - 1;
  }

  public void getDataPoints(double[] xvals, double[] yvals) {
    System.arraycopy(this.xvals, 0, xvals, 0, nPoints);
    System.arraycopy(this.yvals, 0, yvals, 0, nPoints);
  }

  public void setDataPoints(int nPoints, double[] xvals, double[] yvals) {

    if (this.xvals.length < nPoints) {
      this.xvals = new double[nPoints];
      this.yvals = new double[nPoints];
      this.work = new double[2][nPoints];
    }

    this.nPoints = nPoints;
    System.arraycopy(xvals, 0, this.xvals, 0, nPoints);
    System.arraycopy(yvals, 0, this.yvals, 0, nPoints);
  }

  public double[] evaluate(double t, double[] buffer) {

    double c1;
    double c2;
    double denom;

    /*
     * Initialize the work array appropriately.
     */
    System.arraycopy(yvals, 0, work[0], 0, nPoints);

    for (int i = 0; i < nPoints; i++) {
      work[1][i] = 0.0;
    }

    for (int j = 0; j < nPoints - 1; j++) {
      for (int i = 0; i < nPoints - j - 1; i++) {

        denom = xvals[i] - xvals[i + j + 1];

        c1 = t - xvals[i + j + 1];
        c2 = xvals[i] - t;

        work[1][i] =
            ((c1 * work[1][i] + c2 * work[1][i + 1]) + (work[0][i] - work[0][i + 1])) / denom;

        work[0][i] = (c1 * work[0][i] + c2 * work[0][i + 1]) / denom;

      }
    }

    buffer[0] = work[0][0];
    buffer[1] = work[1][0];

    return buffer;

  }

  @Override
  public double differentiate(double t) {
    evaluate(t, tmp);
    return tmp[1];
  }

  @Override
  public double evaluate(double t) {
    evaluate(t, tmp);
    return tmp[0];
  }

}
