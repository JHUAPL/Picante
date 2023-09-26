package picante.math.functions;

import java.util.Arrays;

public class ChebyshevPolynomial implements DifferentiableUnivariateFunction {

  private double[] coeffs;
  private final double[] x2s = new double[2];
  private int degree;

  private double[] w = new double[3];
  private double[] dw = new double[3];
  private double[] tmp = new double[2];

  public ChebyshevPolynomial() {
    this.degree = 1;
    this.coeffs = new double[2];
    x2s[0] = 0.0;
    x2s[1] = 1.0;
  }

  public ChebyshevPolynomial(int degree, double[] coeffs, double[] x2s) {
    this.degree = degree;
    this.coeffs = new double[degree + 1];
    System.arraycopy(coeffs, 0, this.coeffs, 0, degree + 1);
    System.arraycopy(x2s, 0, this.x2s, 0, 2);
  }

  public ChebyshevPolynomial(ChebyshevPolynomial polynomial) {
    this(polynomial.degree, polynomial.coeffs, polynomial.x2s);
  }

  public int getNumberOfCoefficients() {
    return degree + 1;
  }

  public int getDegree() {
    return degree;
  }

  public void getCoefficients(double[] coeffs, double[] x2s) {
    getCoefficients(coeffs, 0, x2s, 0);
  }

  public void getCoefficients(double[] coeffs, int coeffsOffset, double[] x2s, int x2sOffset) {
    System.arraycopy(this.coeffs, 0, coeffs, coeffsOffset, degree + 1);
    System.arraycopy(this.x2s, 0, x2s, x2sOffset, 2);
  }

  public void setCoefficients(ChebyshevPolynomial polynomial) {
    setCoefficients(polynomial.degree, polynomial.coeffs, polynomial.x2s);
  }

  public void setCoefficients(int degree, double[] coeffs, double[] x2s) {
    setCoefficients(degree, coeffs, 0, x2s, 0);
  }

  public void setCoefficients(int degree, double[] coeffs, int coeffsOffset, double[] x2s,
      int x2sOffset) {

    if (this.coeffs.length < degree + 1) {
      this.coeffs = new double[degree + 1];
    }
    this.degree = degree;

    System.arraycopy(coeffs, coeffsOffset, this.coeffs, 0, degree + 1);
    System.arraycopy(x2s, x2sOffset, this.x2s, 0, 2);
  }

  public double[] evaluate(double t, double[] buffer) {

    double s = (t - x2s[0]) / x2s[1];
    double s2 = 2.0 * s;
    int j = degree + 1;
    w[0] = 0.0;
    w[1] = 0.0;
    dw[0] = 0.0;
    dw[1] = 0.0;

    while (j > 1) {

      w[2] = w[1];
      w[1] = w[0];
      w[0] = coeffs[j - 1] + (s2 * w[1] - w[2]);

      dw[2] = dw[1];
      dw[1] = dw[0];
      dw[0] = w[1] * 2.0 + dw[1] * s2 - dw[2];

      j--;
    }

    buffer[0] = coeffs[0] + (s * w[0] - w[1]);
    buffer[1] = w[0] + s * dw[0] - dw[1];
    buffer[1] /= x2s[1];

    return buffer;
  }

  @Override
  public double evaluate(double t) {

    double s = (t - x2s[0]) / x2s[1];
    double s2 = 2.0 * s;
    int j = degree + 1;
    w[0] = 0.0;
    w[1] = 0.0;

    while (j > 1) {
      w[2] = w[1];
      w[1] = w[0];
      w[0] = coeffs[j - 1] + (s2 * w[1] - w[2]);
      j--;
    }

    return (s * w[0] - w[1]) + coeffs[0];
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

    result = prime * result + degree;
    /*
     * This code was taken from Arrays.hashCode(double[]), only adapted to consider only the valid
     * elements of the double array.
     * 
     * result = prime * result + Arrays.hashCode(coeffs);
     */
    int coeffsCode = 1;
    for (int i = 0; i < degree + 1; i++) {
      long bits = Double.doubleToLongBits(coeffs[i]);
      coeffsCode = 31 * result + (int) (bits ^ (bits >>> 32));
    }
    result = prime * result + coeffsCode;
    result = prime * result + Arrays.hashCode(x2s);
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
    ChebyshevPolynomial other = (ChebyshevPolynomial) obj;
    if (degree != other.degree) {
      return false;
    }

    /*
     * Arrays.equals(double[], double[]) is not sufficient, because the arrays may be of different
     * lengths, but contain the same significant content.
     */
    for (int i = 0; i < degree + 1; i++) {
      if (Double.doubleToLongBits(coeffs[i]) != Double.doubleToLongBits(other.coeffs[i])) {
        return false;
      }
    }

    if (!Arrays.equals(x2s, other.x2s)) {
      return false;
    }
    return true;
  }

}
