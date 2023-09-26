package picante.math.functions;

import java.util.List;


/**
 * this class provides ways to create fast polynomials; for quadratics or less, a custom class is
 * created, and for order 3 or higher, a generic class is used, but even this class uses a fast
 * implementation of the Math.pow() function to keep it as numerically speedy and stable as possible
 * 
 * @author vandejd1
 */
public class Polynomials {

  /**
   * @param coefs in order from smallest to highest exponent of independent variable
   */
  public static Polynomial create(double[] coefs) {
    if (coefs.length == 1) {
      return new Constant(coefs[0]);
    } else if (coefs.length == 2) {
      return new Line(coefs[1], coefs[0]);
    } else if (coefs.length == 3) {
      return new Quadratic(coefs[0], coefs[1], coefs[2]);
    }

    return new UnivariatePolynomial(coefs);
  }

  public static Polynomial create(List<Double> coefs) {
    double[] coefsArray = toArray(coefs);
    return create(coefsArray);
  }

  /*
   * used internally to convert List<Double> to an array
   */
  private static double[] toArray(List<Double> list) {
    int n = list.size();
    double[] array = new double[n];
    for (int i = 0; i < n; i++) {
      array[i] = list.get(i);
    }
    return array;
  }

  public static Line createLine(double slope, double intercept) {
    return new Line(slope, intercept);
  }

  public static Line createLine(double x1, double y1, double x2, double y2) {
    double slope = (y2 - y1) / (x2 - x1);

    // plug the (x1, y1) point in and solve for the intercept
    // y = m*x + b
    // b = y - m * x
    double intercept = y1 - slope * x1;

    return new Line(slope, intercept);
  }

  /**
   * returns a line that is perpendicular to the given line and passes through the given point x0,y0
   * 
   * @param line
   * @param x0
   * @param y0
   * @return
   */
  public static Line createPerpendicularLine(Line line, double x0, double y0) {
    // If a line is given by y = m*x + b, then a perpendicular line that passes through
    // the point x0, y0 is given by:
    //
    // y = (x0-x)/m + y0 = (-1/m) x + (y0 + x0/m)

    double slope = -1.0 / line.slope;
    double intercept = y0 + x0 / line.slope;
    return new Line(slope, intercept);
  }


  public static Quadratic createQuadratic(double a0, double a1, double a2) {
    return new Quadratic(a0, a1, a2);
  }

  public static class Constant implements Polynomial {
    private final double constant;

    public Constant(double constant) {
      super();
      this.constant = constant;
    }

    @Override
    public int getDegree() {
      return 0;
    }

    public double[] getCoefficients() {
      return new double[] {constant};
    }

    @Override
    public double evaluate(@SuppressWarnings("unused") double x) {
      return constant;
    }

    @Override
    public double differentiate(@SuppressWarnings("unused") double x) {
      return 0.0;
    }
  }

  public static class Line implements Polynomial {
    private final double slope, intercept;

    public Line(double slope, double intercept) {
      super();
      this.slope = slope;
      this.intercept = intercept;
    }

    @Override
    public int getDegree() {
      return 1;
    }

    public double[] getCoefficients() {
      return new double[] {intercept, slope};
    }

    @Override
    public double evaluate(double x) {
      return slope * x + intercept;
    }

    @Override
    public double differentiate(@SuppressWarnings("unused") double x) {
      return slope;
    }
  }

  public static class Quadratic implements Polynomial {
    private final double a0, a1, a2;

    public Quadratic(double a0, double a1, double a2) {
      super();
      this.a0 = a0;
      this.a1 = a1;
      this.a2 = a2;
    }

    @Override
    public int getDegree() {
      return 2;
    }

    public double[] getCoefficients() {
      return new double[] {a0, a1, a2};
    }


    @Override
    public double evaluate(double x) {
      return a0 + a1 * x + a2 * x * x;
    }

    @Override
    public double differentiate(double x) {
      return a1 + 2.0 * a2 * x;
    }
  }

}
