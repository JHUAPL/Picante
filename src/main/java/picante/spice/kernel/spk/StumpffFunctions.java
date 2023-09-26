package picante.spice.kernel.spk;

import static com.google.common.base.Preconditions.checkArgument;

/**
 * Internal implementation of Stumpff functions to support SPK type 5.
 * <p>
 * This code is adapted from SPICELIB's STMP03.
 * </p>
 */
class StumpffFunctions {

  /**
   * Number of terms in the Maclaurin series for C3. This is the smallest integer such that:
   * 
   * <pre>
   *              1
   *    1.0 + ---------- = 1.0
   *          (2*TRUNC)!
   * </pre>
   */
  private static final int TRUNC = 11;

  /**
   * Used to declare space for the Maclaurin series coefficients and determining how many terms to
   * use in the computation of C2 and C3.
   */
  private static final int NPAIRS = 2 * TRUNC - 2;
  private static final int LPAIR3 = NPAIRS;
  private static final int LPAIR2 = NPAIRS - 1;

  /**
   * Reciprocal pairs, computed once when this class is loaded. Note: as this code was directly
   * adapted from the SPICELIB STMP03 routine, I opted to stick with the FORTRAN array indexing
   * schema. PAIRS[0] is unused as a result.
   */
  private static final double[] PAIRS;

  private static final double LBOUND;

  static {
    PAIRS = new double[NPAIRS + 1];

    for (int i = 1; i <= NPAIRS; i++) {
      PAIRS[i] = 1.0 / ((double) (i) * (double) (i + 1));
    }

    double y = Math.log(2.0) + Math.log(Double.MAX_VALUE);
    LBOUND = -y * y;
  }

  /**
   * Evaluates C0, C1, C2, C3 Stumpff functions at the specified point.
   * 
   * @param x the point to at which to evaluate the functions
   * @param buffer a double precision array of at least length 4 to capture the results.
   * 
   * @return reference to supplied buffer for convenience
   */
  static double[] evaluate(double x, double[] buffer) {

    /**
     * Verify that the input argument exceeds LBOUND, otherwise stop attempting to perform the
     * computation.
     */
    checkArgument(x > LBOUND, "Input value of x must be greater than LBOUND");

    if (x < -1.0) {
      double z = Math.sqrt(-x);
      buffer[0] = Math.cosh(z);
      buffer[1] = Math.sinh(z) / z;
      buffer[2] = (1 - buffer[0]) / x;
      buffer[3] = (1 - buffer[1]) / x;
      return buffer;
    }

    if (x > 1.0) {
      double z = Math.sqrt(x);
      buffer[0] = Math.cos(z);
      buffer[1] = Math.sin(z) / z;
      buffer[2] = (1 - buffer[0]) / x;
      buffer[3] = (1 - buffer[1]) / x;
      return buffer;
    }

    buffer[3] = 1.0;

    for (int i = LPAIR3; i >= 4; i -= 2) {
      buffer[3] = 1.0 - x * PAIRS[i] * buffer[3];
    }

    buffer[3] *= PAIRS[2];

    buffer[2] = 1.0;
    for (int i = LPAIR2; i >= 3; i -= 2) {
      buffer[2] = 1.0 - x * PAIRS[i] * buffer[2];
    }

    buffer[2] *= PAIRS[1];

    buffer[1] = 1.0 - x * buffer[3];
    buffer[0] = 1.0 - x * buffer[2];


    return buffer;
  }
}
