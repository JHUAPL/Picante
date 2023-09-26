package picante.spice.kernel.spk;

import static com.google.common.base.Preconditions.checkArgument;

import java.math.RoundingMode;

import com.google.common.math.DoubleMath;
import picante.math.vectorspace.UnwritableMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;
import picante.units.FundamentalPhysicalConstants;

class EquinoctialElements {

  /**
   * Reference epoch in seconds
   */
  private final double t0;

  /**
   * Semi-major axis of orbit
   */
  private final double a;

  /**
   * The value of H at t0: E*sin(ARGP+NODE)
   */
  private final double h0;

  /**
   * The value of K at t0: E*cos(ARGP+NODE)
   */
  private final double k0;

  /**
   * Mean longitude in radians: MEAN0+ARGP+NODE
   */
  private final double l;

  /**
   * The value of P at t0: tan(INC/2)*sin(NODE)
   */
  private final double p0;

  /**
   * The value of Q at t0: tan(INC/2)*cos(NODE)
   */
  private final double q0;

  /**
   * The rate of the longitude of periapse (dARGP/dt + dNODE/dt) at t0. This rate is assumed to be
   * constant and is measured in radians/second.
   */
  private final double dlpDt;

  /**
   * The derivative of the mean longitude (dM/dt + dARGP/dt + dNODE/dt). This rate is assumed to be
   * constant and is measured in radians/second.
   */
  private final double mlDt;

  /**
   * The rate of the longitude of the ascending node (dNODE/dt), measured in radians/second.
   */
  private final double nodeDt;

  private final double eccentricity;
  private final UnwritableMatrixIJK trans;

  /**
   * 
   * @param t0
   * @param a
   * @param h0
   * @param k0
   * @param l
   * @param p0
   * @param q0
   * @param dlpDt
   * @param mlDt
   * @param nodeDt
   * @param raPole Right ascension of the pole of the reference plane with respect to some inertial
   *        frame measured in radians.
   * @param decPole Declination of the pole of the reference plane with respect to some inertial
   *        frame measured in radians.
   */
  EquinoctialElements(double t0, double a, double h0, double k0, double l, double p0, double q0,
      double dlpDt, double mlDt, double nodeDt, double raPole, double decPole) {
    super();

    checkArgument(a > 0, "Semi-major axis must be strictly positive. Was %s", a);

    this.t0 = t0;
    this.a = a;
    this.h0 = h0;
    this.k0 = k0;
    this.l = l;
    this.p0 = p0;
    this.q0 = q0;
    this.dlpDt = dlpDt;
    this.mlDt = mlDt;
    this.nodeDt = nodeDt;

    this.eccentricity = Math.sqrt(h0 * h0 + k0 * k0);

    checkArgument(eccentricity <= 0.9,
        "Eccentricity of the elements must be less than or equal to 0.9");

    double sa = Math.sin(raPole);
    double ca = Math.cos(raPole);
    double sd = Math.sin(decPole);
    double cd = Math.cos(decPole);

    trans = new UnwritableMatrixIJK(-sa, ca, 0.0, -ca * sd, -sa * sd, cd, ca * cd, sa * cd, sd);

    // trans = new UnwritableMatrixIJK(-sa, -ca * sd, ca * cd, ca, -sa * sd, sa * cd, 0.0, cd, sd);


  }

  VectorIJK evaluate(double t, VectorIJK buffer) {

    double dt = t - t0;

    double dlp = dt * dlpDt;
    double can = Math.cos(dlp);
    double san = Math.sin(dlp);

    double h = h0 * can + k0 * san;
    double k = k0 * can - h0 * san;

    double node = dt * nodeDt;

    double cn = Math.cos(node);
    double sn = Math.sin(node);

    double p = p0 * cn + q0 * sn;
    double q = q0 * cn - p0 * sn;

    double b = Math.sqrt(1.0 - h * h - k * k);
    b = 1.0 / (1.0 + b);

    double di = 1.0 / (1.0 + p * p + q * q);

    VectorIJK vf = new VectorIJK((1.0 - p * p + q * q) * di, 2.0 * p * q * di, -2.0 * p * di);
    VectorIJK vg = new VectorIJK(2.0 * p * q * di, (1.0 + p * p - q * q) * di, 2.0 * q * di);

    double ml = l + mlDt * dt % FundamentalPhysicalConstants.TWOPI;

    double eecan = keplersEquation(ml, h, k);

    double sf = Math.sin(eecan);
    double cf = Math.cos(eecan);

    double x1 = a * ((1.0 - (b * h * h)) * cf + (h * k * b * sf - k));
    double y1 = a * ((1.0 - (b * k * k)) * sf + (h * k * b * cf - h));

    VectorIJK.combine(x1, vf, y1, vg, buffer);

    trans.mxv(buffer, buffer);

    return buffer;

  }

  StateVector evaluate(double t, StateVector buffer) {

    double dt = t - t0;

    double dlp = dt * dlpDt;
    double can = Math.cos(dlp);
    double san = Math.sin(dlp);

    double h = h0 * can + k0 * san;
    double k = k0 * can - h0 * san;

    double node = dt * nodeDt;

    double cn = Math.cos(node);
    double sn = Math.sin(node);

    double p = p0 * cn + q0 * sn;
    double q = q0 * cn - p0 * sn;

    double pRate = dlpDt - nodeDt;

    double b = Math.sqrt(1.0 - h * h - k * k);
    b = 1.0 / (1.0 + b);

    double di = 1.0 / (1.0 + p * p + q * q);

    VectorIJK vf = new VectorIJK((1.0 - p * p + q * q) * di, 2.0 * p * q * di, -2.0 * p * di);
    VectorIJK vg = new VectorIJK(2.0 * p * q * di, (1.0 + p * p - q * q) * di, 2.0 * q * di);

    double ml = l + mlDt * dt % FundamentalPhysicalConstants.TWOPI;

    double eecan = keplersEquation(ml, h, k);

    double sf = Math.sin(eecan);
    double cf = Math.cos(eecan);

    double x1 = a * ((1.0 - (b * h * h)) * cf + (h * k * b * sf - k));
    double y1 = a * ((1.0 - (b * k * k)) * sf + (h * k * b * cf - h));

    double rb = h * sf + k * cf;
    double r = a * (1.0 - rb);
    double ra = (mlDt * a * a) / r;

    double dx1 = ra * (-sf + h * b * rb);
    double dy1 = ra * (cf - k * b * rb);

    double nfac = 1.0 - (dlpDt / mlDt);

    double dx = nfac * dx1 - pRate * y1;
    double dy = nfac * dy1 + pRate * x1;

    VectorIJK.combine(x1, vf, y1, vg, buffer.getPosition());

    VectorIJK temp = new VectorIJK(-nodeDt * buffer.getPosition().getJ(),
        nodeDt * buffer.getPosition().getI(), 0.0);

    VectorIJK.combine(1.0, temp, dx, vf, dy, vg, buffer.getVelocity());

    trans.mxv(buffer.getPosition(), buffer.getPosition());
    trans.mxv(buffer.getVelocity(), buffer.getVelocity());


    return buffer;

  }

  private static final int MAXIMUM_NEWTON_ITERATIONS = 5;


  /**
   * 
   * <p>
   * This is adapted directly from NAIF's KEPLEQ routine.
   * </p>
   * 
   * @param ml
   * @param h0
   * @param k0
   * @return
   */
  static double keplersEquation(double ml, double h0, double k0) {
    double h = -h0 * Math.cos(ml) + k0 * Math.sin(ml);
    double k = h0 * Math.sin(ml) + k0 * Math.cos(ml);

    return ml + solveKeplersEquation(h, k);
  }

  /**
   * 
   * <p>
   * This is adapted directly from NAIF's KPSOLV routine.
   * </p>
   */
  static double solveKeplersEquation(double h, double k) {
    double ecc2 = h * h + k * k;

    /*
     * Solve Kepler's equation.
     */
    double y0 = -h;
    double xm = 0.0;
    double ecc = Math.sqrt(ecc2);

    double xu, xl;

    if (y0 > 0.0) {
      xu = 0.0;
      xl = -ecc;
    } else if (y0 < 0.0) {
      xu = ecc;
      xl = 0.0;
    } else {
      return 0.0;
    }

    /*
     * Iterate until we are assured of being a region where Newton's method will converge quickly.
     */
    int maxit =
        Math.min(32, Math.max(1, DoubleMath.roundToInt(1.0 / (1.0 - ecc), RoundingMode.HALF_UP)));

    for (int i = 0; i < maxit; i++) {

      xm = Math.max(xl, Math.min(xu, 0.5 * (xl + xu)));

      double yxm = xm - h * Math.cos(xm) - k * Math.sin(xm);

      if (yxm > 0.0) {
        xu = xm;
      } else {
        xl = xm;
      }

    }

    /*
     * We've bisected into a region where we can now get rapid convergence using Newton's method.
     */
    double x = xm;

    for (int i = 0; i < MAXIMUM_NEWTON_ITERATIONS; i++) {
      double cosx = Math.cos(x);
      double sinx = Math.sin(x);

      double yx = x - h * cosx - k * sinx;
      double ypx = 1.0 + h * sinx - k * cosx;
      x = x - yx / ypx;
    }

    return x;

  }

}
