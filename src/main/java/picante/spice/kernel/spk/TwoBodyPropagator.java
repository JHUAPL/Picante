package picante.spice.kernel.spk;

import static com.google.common.base.Preconditions.checkArgument;

import com.google.common.primitives.Doubles;
import picante.math.intervals.Interval;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;
import picante.mechanics.UnwritableStateVector;

/**
 * Internal implementation of a two-body propagator in support of SPK type 5.
 * <p>
 * This code is adapted from SPICELIB's PROP2B
 * </p>
 * <p>
 * The mutability of this class is less than ideal, but it mimics the structure of the existing SPK
 * type implementations.
 * </p>
 */
class TwoBodyPropagator {

  private static final int MAXBIT = 64;

  private final VectorIJK position = new VectorIJK();
  private final VectorIJK velocity = new VectorIJK();
  private final Interval bounds = new Interval();
  private double t0;
  private double b2rv;
  private double bq;
  private double br0;
  private double f;
  private double qovr0;

  TwoBodyPropagator() {}

  /**
   * Configures the internal state of this instance to propagate the new state, GM and t0
   * combination.
   * 
   * @param gm
   * @param t0
   * @param state
   */
  void configure(double gm, double t0, UnwritableStateVector state) {

    checkArgument(gm > 0, "gm must be strictly positive");
    checkArgument(!state.getPosition().equals(VectorIJK.ZERO),
        "position must not be the zero vector");
    checkArgument(!state.getVelocity().equals(VectorIJK.ZERO),
        "velocity must not be the zero vector");

    this.t0 = t0;
    this.position.setTo(state.getPosition());
    this.velocity.setTo(state.getVelocity());

    double r0 = state.getPosition().getLength();
    double rv = state.getPosition().getDot(state.getVelocity());

    /*
     * Let hvec be the specific angular momentum vector, and let q be the distance at periapse.
     */
    VectorIJK hvec = VectorIJK.cross(state.getPosition(), state.getVelocity());
    double h2 = hvec.getDot(hvec);

    checkArgument(h2 != 0, "Non-conic (rectilinear) motion is not allowed");

    VectorIJK tmpvec = VectorIJK.cross(state.getVelocity(), hvec);
    VectorIJK eqvec = VectorIJK.combine(1.0 / gm, tmpvec, -1.0 / r0, state.getPosition());
    double e = eqvec.getLength();

    /*
     * Solve the equation H2 = gm*q*(1+e) for q.
     */
    double q = h2 / (gm * (1.0 + e));

    f = 1.0 - e;
    double b = Math.sqrt(q / gm);
    br0 = b * r0;
    b2rv = b * b * rv;
    bq = b * q;

    qovr0 = q / r0;

    double maxc =
        Doubles.max(1.0, Math.abs(br0), Math.abs(b2rv), Math.abs(bq), Math.abs(qovr0 / bq));

    double bound;
    if (f < 0) {
      double logmxc = Math.log(maxc);
      double logdpm = Math.log(Double.MAX_VALUE / 2.0);
      double fixed = logdpm - logmxc;
      double rootf = Math.sqrt(-f);
      double logf = Math.log(-f);
      bound = Math.min(fixed / rootf, (fixed + 1.5 * logf) / rootf);
    } else {
      double logbnd = (Math.log(1.5) + Math.log(Double.MAX_VALUE) - Math.log(maxc)) / 3.0;
      bound = Math.exp(logbnd);
    }

    this.bounds.set(-bound, bound);

  }

  public double getT0() {
    return t0;
  }

  public StateVector getState(double time, StateVector buffer) {

    buffer = (buffer == null) ? new StateVector() : buffer;

    double dt = time - t0;

    double x = dt / bq;
    x = bounds.clamp(x);

    double fx2 = f * x * x;

    double[] stumpff = new double[4];
    StumpffFunctions.evaluate(fx2, stumpff);

    double kfun = x * (br0 * stumpff[1] + x * (b2rv * stumpff[2] + x * (bq * stumpff[3])));

    double lower;
    double upper;

    if (dt < 0) {

      upper = 0;
      lower = x;

      while (kfun > dt) {

        upper = lower;
        lower *= 2.0;
        double oldx = x;
        x = bounds.clamp(lower);

        if (x == oldx) {
          // fx2 = f * bound * bound;
          // StumpffFunctions.evaluate(fx2, stumpff);
          // double kfunl =
          // -bound * (br0 * stumpff[1] - bound * (b2rv * stumpff[2] - bound * bq * stumpff[3]));
          // double kfunu =
          // bound * (br0 * stumpff[1] + bound * (b2rv * stumpff[2] + bound * bq * stumpff[3]));
          throw new IllegalArgumentException(
              "Unable to reliably propagate state, time delta too large.");
        }

        fx2 = f * x * x;
        StumpffFunctions.evaluate(fx2, stumpff);
        kfun = x * (br0 * stumpff[1] + x * (b2rv * stumpff[2] + x * (bq * stumpff[3])));

      }

    } else if (dt > 0) {

      lower = 0;
      upper = x;

      while (kfun < dt) {

        lower = upper;
        upper *= 2.0;

        double oldx = x;
        x = bounds.clamp(upper);

        if (x == oldx) {
          throw new IllegalArgumentException(
              "Unable to reliably propagate state, time delta too large.");
        }

        fx2 = f * x * x;

        StumpffFunctions.evaluate(fx2, stumpff);
        kfun = x * (br0 * stumpff[1] + x * (b2rv * stumpff[2] + x * bq * stumpff[3]));

      }

    } else {
      buffer.setPosition(position);
      buffer.setVelocity(velocity);
      return buffer;
    }

    /*
     * The root's bracketed.
     */
    x = Math.min(upper, Math.max(lower, (lower + upper) / 2.0));
    fx2 = f * x * x;

    StumpffFunctions.evaluate(fx2, stumpff);

    int lcount = 0;
    int mostc = 1000;

    while ((x > lower) && (x < upper) && (lcount < mostc)) {

      kfun = x * (br0 * stumpff[1] + x * (b2rv * stumpff[2] + x * bq * stumpff[3]));

      if (kfun > dt) {
        upper = x;
      } else if (kfun < dt) {
        lower = x;
      } else {
        upper = x;
        lower = x;
      }

      /*
       * As soon as the bracketing values move away from zero, modify the count limit.
       */
      if (mostc > MAXBIT) {

        if ((upper != 0.0) && (lower != 0.0)) {
          mostc = MAXBIT;
          lcount = 0;
        }

      }

      x = Math.min(upper, Math.max(lower, (lower + upper) / 2.0));
      fx2 = f * x * x;

      StumpffFunctions.evaluate(fx2, stumpff);
      lcount++;

    }

    /*
     * With x in hand, simply compute br, pc, vc, pcdot and vcdot.
     */
    double x2 = x * x;
    double x3 = x2 * x;
    double br = br0 * stumpff[0] + x * (b2rv * stumpff[1] + x * (bq * stumpff[2]));

    double pc = 1.0 - qovr0 * x2 * stumpff[2];
    double vc = dt - bq * x3 * stumpff[3];
    double pcdot = -(qovr0 / br) * x * stumpff[1];
    double vcdot = 1.0 - (bq / br) * x2 * stumpff[2];

    VectorIJK.combine(pc, position, vc, velocity, buffer.getPosition());
    VectorIJK.combine(pcdot, position, vcdot, velocity, buffer.getVelocity());

    return buffer;
  }



}
