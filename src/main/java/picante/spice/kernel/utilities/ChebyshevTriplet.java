package picante.spice.kernel.utilities;

import picante.math.functions.ChebyshevPolynomial;

/**
 * Interface defining a method to set the contents of a record containing three Chebyshev polynomial
 * coefficient sets. This interface was used to consolidate the DAF access mechanism for the Type2
 * PCK and SPK implementations.
 */
public interface ChebyshevTriplet {

  /**
   * Sets the coefficients of each of the polynomials in the triplet.
   * 
   * @param degree the degree of all three polynomials
   * @param firstCoeffs the array containing the coefficients for the first of the polynomial
   *        triplets
   * @param firstCoeffsOffset the starting index of the first coefficients in the firstCoeffs array
   * @param secondCoeffs the array containing the coefficients for the second of the polynomial
   *        triplets
   * @param secondCoeffsOffset the starting index of the second coefficients in the secondCoeffs
   *        array
   * @param thirdCoeffs the array containing the coefficients for the third of the polynomial
   *        triplets
   * @param thirdCoeffsOffset the starting index of the third coefficients in the thirdCoeffs array
   * @param x2s the array containing the transformation from actual domain to standard Chebyshev
   *        domain parameters
   * @param x2sOffset the starting index of the x2s coefficients in the x2s array
   * 
   * @see ChebyshevPolynomial
   */
  public void setCoefficients(int degree, double[] firstCoeffs, int firstCoeffsOffset,
      double[] secondCoeffs, int secondCoeffsOffset, double[] thirdCoeffs, int thirdCoeffsOffset,
      double[] x2s, int x2sOffset);

}
