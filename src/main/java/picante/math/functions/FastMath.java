package picante.math.functions;

/**
 * copied here from Apache commons math is the FastMAth implementation of pow(double, int) which is
 * needed for the {@link UnivariatePolynomial} implementation
 * 
 * @author vandejd1
 * 
 *         Here is the required NOTICE.txt attributions for using commons-math-3 version 3.5:
 * 
 *         Apache Commons Math Copyright 2001-2015 The Apache Software Foundation
 * 
 *         This product includes software developed at The Apache Software Foundation
 *         (http://www.apache.org/).
 * 
 *         This product includes software developed for Orekit by CS Systemes d'Information
 *         (http://www.c-s.fr/) Copyright 2010-2012 CS Systemes d'Information
 * 
 */
class FastMath {
  /**
   * Raise a double to an int power.
   *
   * @param d Number to raise.
   * @param e Exponent.
   * @return d<sup>e</sup>
   * @since 3.1
   */
  public static double pow(double d, int e) {

    if (e == 0) {
      return 1.0;
    } else if (e < 0) {
      e = -e;
      d = 1.0 / d;
    }

    // split d as two 26 bits numbers
    // beware the following expressions must NOT be simplified, they rely on floating point
    // arithmetic properties
    final int splitFactor = 0x8000001;
    final double cd = splitFactor * d;
    final double d1High = cd - (cd - d);
    final double d1Low = d - d1High;

    // prepare result
    double resultHigh = 1;
    double resultLow = 0;

    // d^(2p)
    double d2p = d;
    double d2pHigh = d1High;
    double d2pLow = d1Low;

    while (e != 0) {

      if ((e & 0x1) != 0) {
        // accurate multiplication result = result * d^(2p) using Veltkamp TwoProduct algorithm
        // beware the following expressions must NOT be simplified, they rely on floating point
        // arithmetic properties
        final double tmpHigh = resultHigh * d2p;
        final double cRH = splitFactor * resultHigh;
        final double rHH = cRH - (cRH - resultHigh);
        final double rHL = resultHigh - rHH;
        final double tmpLow =
            rHL * d2pLow - (((tmpHigh - rHH * d2pHigh) - rHL * d2pHigh) - rHH * d2pLow);
        resultHigh = tmpHigh;
        resultLow = resultLow * d2p + tmpLow;
      }

      // accurate squaring d^(2(p+1)) = d^(2p) * d^(2p) using Veltkamp TwoProduct algorithm
      // beware the following expressions must NOT be simplified, they rely on floating point
      // arithmetic properties
      final double tmpHigh = d2pHigh * d2p;
      final double cD2pH = splitFactor * d2pHigh;
      final double d2pHH = cD2pH - (cD2pH - d2pHigh);
      final double d2pHL = d2pHigh - d2pHH;
      final double tmpLow =
          d2pHL * d2pLow - (((tmpHigh - d2pHH * d2pHigh) - d2pHL * d2pHigh) - d2pHH * d2pLow);
      final double cTmpH = splitFactor * tmpHigh;
      d2pHigh = cTmpH - (cTmpH - tmpHigh);
      d2pLow = d2pLow * d2p + tmpLow + (tmpHigh - d2pHigh);
      d2p = d2pHigh + d2pLow;

      e >>= 1;

    }

    return resultHigh + resultLow;

  }

}
