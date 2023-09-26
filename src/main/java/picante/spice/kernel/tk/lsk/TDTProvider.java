package picante.spice.kernel.tk.lsk;

/**
 * Internal package implementation class that provides TDT conversion services for the leapseconds
 * kernel.
 */
class TDTProvider implements UniformTimeProvider {

  /**
   * According to the NAIF documentation in UNITIM, 3 iterations of the contraction map is
   * sufficient to invert the conversion formula with enough accuracy for most celestial
   * applications.
   */
  private static final int STANDARD_ITERATIONS = 3;

  private final double k;
  private final double eb;
  private final double[] m;
  private final int iterations;

  public TDTProvider(double k, double eb, double[] m) {
    this(k, eb, m, STANDARD_ITERATIONS);
  }

  public TDTProvider(double k, double eb, double[] m, int iterations) {
    this.k = k;
    this.eb = eb;
    this.m = m;
    this.iterations = iterations;
  }

  @Override
  public double convertToTDB(double uniformTime) {
    return uniformTime
        + k * Math.sin(m[0] + m[1] * uniformTime + eb * Math.sin(m[0] + m[1] * uniformTime));
  }

  /**
   * {@inheritDoc}
   * 
   * <p>
   * Computation Details: The following text is taken from the SPICELIB routine UNITIM. It provides
   * a detailed explanation of why the algorithm used to invert the TDT to TDB conversion works.
   * </p>
   * <p>
   * What we have to do here is invert the formula used to get TDB from TDT that was used above. Of
   * course solving the equation:
   * 
   * <pre>
           TDB = TDT + K*SIN { M0 + M1*TDT + EB*SIN( MO + M1*TDT ) }
   * </pre>
   * 
   * analytically for TDT if given TDB is no piece of cake. However, we can get as close as we want
   * to TDT if we notice a few tricks. First, let's let f(t) denote the function:
   * 
   * <pre>
   * f(t) = SIN(M0 + M1 * t + EB * SIN(M0 + M1 * t))
   * </pre>
   * 
   * With this simpler notation we can rewrite our problem as that of solving the equation:
   * 
   * <pre>
   * y = t + K * f(t)
   * </pre>
   * 
   * for t given y. Whichever t satisfies this equation will be unique. The uniqueness of the
   * solution is ensured because the expression on the right-hand side of the equation is monotone
   * increasing in t.
   * </p>
   * <p>
   * Let's suppose that t is the solution, then the following is true.
   * 
   * <pre>
   * t = y - K * f(t)
   * </pre>
   * 
   * but we can also replace the t on the right hand side of the equation by <code>y - K*f(t)</code>
   * . Thus:
   * 
   * <pre>
           t = y - K*f( y - K*f(t))
       
             = y - K*f( y - K*f( y - K*f(t)))
  
             = y - K*f( y - K*f( y - K*f( y - K*f(t))))
  
             = y - K*f( y - K*f( y - K*f( y - K*f( y - K*f(t)))))
             .
             .
             .
             = y - K*f( y - K*f( y - K*f( y - K*f( y - K*f(y - ... )))
   * </pre>
   * 
   * and so on, for as long as we have patience to perform the substitutions.
   * </p>
   * <p>
   * The point of doing this recursive substitution is that we hope to move t to an insignificant
   * part of the computation. This would seem to have a reasonable chance of success since K is a
   * small number and f is bounded by 1.
   * </p>
   * <p>
   * Following this idea, we will attempt to solve for t using the recursive method outlined below.
   * We will make our first guess at t, call it t_0:
   * 
   * <pre>
   * t_0 = y
   * </pre>
   * 
   * Our next guess, t_1, is given by:
   * 
   * <pre>
   * t_1 = y - K * f(t_0)
   * </pre>
   * 
   * And so on:
   * 
   * <pre>
         t_2 = y - K*f(t_1)        [ = y - K*f(y - K*f(y))            ]
         t_3 = y - K*f(t_2)        [ = y - K*f(y - K*f(y - K*f(y)))   ]
             .
             .
             .
         t_n = y - K*f(t_(n-1))    [ = y - K*f(y - K*f(y - K*f(y...)))]
   * </pre>
   * </p>
   * 
   * The questions to ask at this point are:
   * <ol>
   * <li>Do the t_i's converge?</li>
   * <li>If they converge, do they converge to t?</li>
   * <li>If they converge to t, how fast do they get there?</li>
   * </ol>
   * <ol>
   * <li>The sequence of approximations converges.
   * 
   * <pre>
           | t_n - t_(n-1) | =    [ y - K*f( t_(n-1) ) ]
                               -  [ y - K*f( t_(n-2) ) ]
  
                             =  K*[ f( t_(n-2) ) - f( t_(n-1) ) ]
   * </pre>
   * 
   * The function f has an important property. The absolute value of its derivative is always less
   * than <code>M1*(1+EB)</code>. This means that for any pair of real numbers s,t
   * 
   * <pre>
              | f(t) - f(s) |  < M1*(1+EB)*| t - s |.
   * </pre>
   * 
   * From this observation, we can see that
   * 
   * <pre>
             | t_n - t_(n-1) | < K*M1*(1+EB)*| t_(n-1) - t_(n-2) |
   * </pre>
   * 
   * With this fact available, we could (with a bit more work) conclude that the sequence of t_i's
   * converges and that it converges at a rate that is at least as fast as the sequence L, L**2,
   * L**3, ....
   * <p>
   * Where <code>L = K*M1*(1+EB) << 1</code>.
   * </p>
   * </li>
   * <li>If we let t be the limit of the t_i's then it follows that
   * 
   * <pre>
               t = y - K*f(t).
   * </pre>
   * 
   * or that
   * 
   * <pre>
               y = t + K*f(t).
   * </pre>
   * 
   * </li>
   * <li>As we already pointed out, the sequence of t_i's converges at least as fast as the
   * geometric series L, L**2, ...
   * <p>
   * Since <code>K*M1*(1+EB)</code> is quite small (on the order of 10**-9) 3 iterations should get
   * us as close as we can get to the solution for TDT
   * </p>
   * </pre>
   * </li>
   * </ol>
   */
  @Override
  public double convertToUniformTime(double tdb) {
    double result = tdb;

    for (int i = 0; i < iterations; i++) {
      result = tdb - k * Math.sin(m[0] + m[1] * result + eb * Math.sin(m[0] + m[1] * result));
    }

    return result;
  }

  public double getK() {
    return k;
  }

  public double getEB() {
    return eb;
  }

  public double[] getM(double[] buffer) {
    System.arraycopy(m, 0, buffer, 0, 2);
    return buffer;
  }

  public int getIterations() {
    return iterations;
  }

}
