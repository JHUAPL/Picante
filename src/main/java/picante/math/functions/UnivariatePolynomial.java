package picante.math.functions;

import static com.google.common.base.Preconditions.checkNotNull;
import static picante.math.PicanteMath.pow;


class UnivariatePolynomial implements Polynomial {
  private final double[] coefs;

  UnivariatePolynomial(double[] coefs) {
    checkNotNull(coefs);
    this.coefs = new double[coefs.length];
    System.arraycopy(coefs, 0, this.coefs, 0, coefs.length);
  }

  @Override
  public int getDegree() {
    return coefs.length - 1;
  }

  public double[] getCoefficients() {
    double[] newCoefArray = new double[coefs.length];
    System.arraycopy(coefs, 0, newCoefArray, 0, coefs.length);
    return newCoefArray;
  }


  @Override
  public double evaluate(double x) {
    double sum = 0.0;
    for (int i = 0; i < coefs.length; i++) {
      sum = sum + coefs[i] * pow(x, i);
    }
    return sum;
  }

  @Override
  public double differentiate(double x) {
    double sum = 0.0;
    for (int i = 1; i < coefs.length; i++) {
      sum = sum + i * coefs[i] * pow(x, i - 1);
    }
    return sum;
  }

}
