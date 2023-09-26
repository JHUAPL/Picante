package picante.math.functions;

/**
 * Hermite polynomial implementation.
 * <p>
 * This class was written largely with a specific focus in mind, so the organization of the internal
 * data and methods used to set and retrieve that information were designed with that specific focus
 * as the only goal. Please do not change the way this class works without talking to me first.
 * </p>
 * <p>
 * There is no checking of the input x-values array. It should be a strictly monotonically
 * increasing sequence of values.
 * </p>
 * 
 */
public class HermitePolynomial implements DifferentiableUnivariateFunction {

  private double[] xvals;
  private double[] yvals;
  private int nPoints;

  private double[][] work;
  private final double[] tmp = new double[2];

  public HermitePolynomial() {
    this.nPoints = 2;
    this.xvals = new double[nPoints];
    this.yvals = new double[2 * nPoints];
    this.xvals[0] = 0.0;
    this.xvals[1] = 1.0;
    this.work = new double[2][2 * nPoints];
  }

  public HermitePolynomial(int nPoints, double[] xvals, double[] yvals) {
    this.nPoints = nPoints;
    this.xvals = new double[nPoints];
    this.yvals = new double[2 * nPoints];
    System.arraycopy(xvals, 0, this.xvals, 0, nPoints);
    System.arraycopy(yvals, 0, this.yvals, 0, 2 * nPoints);

    this.work = new double[2][2 * this.nPoints];
  }

  public HermitePolynomial(HermitePolynomial polynomial) {
    this(polynomial.nPoints, polynomial.xvals, polynomial.yvals);
  }

  public int getNumberOfPoints() {
    return nPoints;
  }

  public int getDegree() {
    return 2 * nPoints - 1;
  }

  public void getPoints(double[] xvals, double[] yvals) {
    getPoints(xvals, 0, yvals, 0);
  }

  public void getPoints(double[] xvals, int xvalsOffset, double[] yvals, int yvalsOffset) {
    System.arraycopy(this.xvals, 0, xvals, xvalsOffset, nPoints);
    System.arraycopy(this.yvals, 0, yvals, yvalsOffset, 2 * nPoints);
  }

  public void setPoints(HermitePolynomial polynomial) {
    setPoints(polynomial.nPoints, polynomial.xvals, polynomial.yvals);
  }

  public void setPoints(int nPoints, double[] xvals, double[] yvals) {
    setPoints(nPoints, xvals, 0, yvals, 0);
  }

  public void setPoints(int nPoints, double[] xvals, int xvalsOffset, double[] yvals,
      int yvalsOffset) {

    /*
     * Expand the arrays held internally, if necessary, to accomodate the new data points.
     */
    if (this.xvals.length < nPoints) {
      this.xvals = new double[nPoints];
      this.yvals = new double[2 * nPoints];
      this.work[0] = new double[2 * nPoints];
      this.work[1] = new double[2 * nPoints];
    }
    this.nPoints = nPoints;

    System.arraycopy(xvals, xvalsOffset, this.xvals, 0, nPoints);
    System.arraycopy(yvals, yvalsOffset, this.yvals, 0, 2 * nPoints);
  }

  public double[] evaluate(double t, double[] buffer) {

    double c1;
    double c2;
    double denom;
    double temp;

    int prev;
    int next;
    int curr;

    /*
     * Initialize the work array appropriately.
     */
    System.arraycopy(yvals, 0, work[0], 0, 2 * nPoints);

    /*
     * Compute the second row of the interpolation table: this consists of the n-1 values obtained
     * by evaluating the first degree interpolants at t. We'll also evaluate the derivatives of
     * these interpolants at t and save the results in the second row of the work array.
     * 
     * Because the derivative computations depend on the function computations in the previous row
     * of the interpolation table, and because the function interpolation overwrites the previous
     * row of values, we must evaluate the derivatives first.
     */
    for (int i = 0; i < nPoints - 1; i++) {

      c1 = xvals[i + 1] - t;
      c2 = t - xvals[i];
      denom = xvals[i + 1] - xvals[i];

      /*
       * The second row of the work array contains the interpolated derivative values. The
       * even-indexed interpolated derivatives are simply the input derivatives.
       */
      prev = 2 * i;
      curr = prev + 1;
      next = curr + 1;

      work[1][prev] = work[0][curr];

      /*
       * The odd-indexed interpolated derivatives are the slopes of the linear interpolating
       * polynomial for adjacent input abscissa/ordinate pairs.
       */
      work[1][curr] = (work[0][next] - work[0][prev]) / denom;

      /*
       * The first row of work contains interpolated function values. The even-indexed entries are
       * the linear Taylor polynomials, each input abscissa value, evaluated at t.
       */
      temp = work[0][curr] * (t - xvals[i]) + work[0][prev];

      work[0][curr] = (c1 * work[0][prev] + c2 * work[0][next]) / denom;
      work[0][prev] = temp;

    }

    /*
     * The last row entries were not computed by the preceding loop.
     */
    work[1][2 * nPoints - 2] = work[0][2 * nPoints - 1];
    work[0][2 * nPoints - 2] =
        work[0][2 * nPoints - 1] * (t - xvals[nPoints - 1]) + work[0][2 * nPoints - 2];

    int xi;
    int xij;

    /*
     * Compute rows 2 through 2*n-1 of the table.
     */
    for (int j = 1; j < 2 * nPoints - 1; j++) {
      for (int i = 0; i < 2 * nPoints - j - 1; i++) {

        /*
         * In the theoretical construction of the interpolation table, there are 2*n abscissa
         * values, since each input abscissa value occurs with multiplicity two. In this theoretical
         * construction, the jth row of the interpolation table contains the results of evaluating
         * the interpolants that span j+1 consecutive abscissa values. The indices xi and xij below
         * are used to pick the correct abscissa values out of the physical xvals array, in which
         * the abscissa values are not repeated.
         */
        xi = i / 2;
        xij = (i + j + 1) / 2;

        c1 = xvals[xij] - t;
        c2 = t - xvals[xi];

        denom = xvals[xij] - xvals[xi];

        /*
         * Compute the interpolated derivative at t for the ith interpolant. This is the derivative
         * with respect to t of the expression for the interpolated function value, which is the
         * second expression below. This derivative computation is done first because it relies on
         * the interpolated function values from the previous row of the interpolation table.
         */
        work[1][i] =
            (c1 * work[1][i] + c2 * work[1][i + 1] + (work[0][i + 1] - work[0][i])) / denom;

        /*
         * Compute the interpolated function value at x for the ith interpolant.
         */
        work[0][i] = (c1 * work[0][i] + c2 * work[0][i + 1]) / denom;
      }
    }

    /*
     * Copy the results to the output buffer.
     */
    buffer[0] = work[0][0];
    buffer[1] = work[1][0];

    return buffer;
  }

  @Override
  public double evaluate(double t) {
    evaluate(t, tmp);
    return tmp[0];
  }

  @Override
  public double differentiate(double t) {
    evaluate(t, tmp);
    return tmp[1];
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + nPoints;

    /*
     * Arrays.hashCode(double[]) is not sufficient because we only wish to compare elements of the
     * array defined up to nPoints. This was taken directly from the source for that method and
     * adapted appropriately.
     */
    int xvalsCode = 1;
    for (int i = 0; i < nPoints; i++) {
      long bits = Double.doubleToLongBits(xvals[i]);
      xvalsCode = 31 * result + (int) (bits ^ (bits >>> 32));
    }
    result = prime * result + xvalsCode;

    int yvalsCode = 1;
    for (int i = 0; i < 2 * nPoints; i++) {
      long bits = Double.doubleToLongBits(yvals[i]);
      yvalsCode = 31 * result + (int) (bits ^ (bits >>> 32));
    }

    result = prime * result + yvalsCode;
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    HermitePolynomial other = (HermitePolynomial) obj;
    if (nPoints != other.nPoints) {
      return false;
    }

    /*
     * Arrays.equals(double[], double[]) is not sufficient, because the content of the arrays may be
     * of different lengths than their allocated storage.
     */
    for (int i = 0; i < nPoints; i++) {
      if (Double.doubleToLongBits(xvals[i]) != Double.doubleToLongBits(other.xvals[i])) {
        return false;
      }
    }
    for (int i = 0; i < 2 * nPoints; i++) {
      if (Double.doubleToLongBits(yvals[i]) != Double.doubleToLongBits(other.yvals[i])) {
        return false;
      }
    }

    return true;
  }

}
