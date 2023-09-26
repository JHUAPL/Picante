package picante.math.vectorspace;

import static picante.math.PicanteMath.abs;
import static picante.math.PicanteMath.max;
import static picante.math.PicanteMath.sqrt;

/**
 * Class providing various static methods that support the implementation of methods of the various
 * other classes provided in this package.
 * <p>
 * <b>Maintainer Note:</b>This class is an implementation detail of the classes and methods provided
 * by this package as a whole. Functionality present here should not be exposed outside of this
 * package. Further, as methods defined here may be invoked from any class in this package, it must
 * remain free of references to all other classes in this package. In other words, it is at the
 * absolute bottom of the package layering structure.
 * </p>
 */
class InternalOperations {

  /**
   * Computes the absolute value of the largest, magnitude, component of a vector expressed in three
   * individual components.
   * 
   * @param i the ith component
   * @param j the jth component
   * @param k the kth component
   * 
   * @return the absolute value of the largest, in magnitude, component of the vector [i,j,k].
   */
  static double absMaxComponent(double i, double j, double k) {
    return max(abs(i), max(abs(j), abs(k)));
  }

  /**
   * Method that computes the norm of a vector expressed as three separate components. This method
   * is marked with package access to allow other classes in the vector arithmetic toolkit to
   * utilize it.
   * 
   * @param i the ith component
   * @param j the jth component
   * @param k the kth component
   * 
   * @return the length of the [i,j,k] vector
   */
  static double computeNorm(double i, double j, double k) {

    double max = absMaxComponent(i, j, k);

    /*
     * If max is 0, then vector is clearly the zero vector.
     */
    if (max == 0.0) {
      return 0.0;
    }

    i /= max;
    j /= max;
    k /= max;

    /*
     * Since we're trying to avoid overflow in the square root:
     */
    return max * sqrt(i * i + j * j + k * k);

  }

  /**
   * Determine if the components of a three by three matrix constitute a rotation.
   * 
   * @param ii ith row, ith column element
   * @param ji jth row, ith column element
   * @param ki kth row, ith column element
   * @param ij ith row, jth column element
   * @param jj jth row, jth column element
   * @param kj kth row, jth column element
   * @param ik ith row, kth column element
   * @param jk jth row, kth column element
   * @param kk kth row, kth column element
   * @param normTolerance tolerance off of unity for the magnitude of the column vectors
   * @param detTolerance tolerance off of unity for the determinant of the matrix
   * 
   * @throws MalformedRotationException if the supplied components do not adequately describe a
   *         rotation given the supplied tolerances
   */
  static void checkRotation(double ii, double ji, double ki, double ij, double jj, double kj,
      double ik, double jk, double kk, double normTolerance, double detTolerance)
      throws MalformedRotationException {

    double testVal = computeNorm(ii, ji, ki);
    if ((testVal < 1.0 - normTolerance) || (testVal > 1.0 + normTolerance)) {
      throw new MalformedRotationException(
          "Matrix's ith column is not sufficiently close to unit length.");
    }

    testVal = computeNorm(ij, jj, kj);
    if ((testVal < 1.0 - normTolerance) || (testVal > 1.0 + normTolerance)) {
      throw new MalformedRotationException(
          "Matrix's jth column is not sufficiently close to unit length.");
    }

    testVal = computeNorm(ik, jk, kk);
    if ((testVal < 1.0 - normTolerance) || (testVal > 1.0 + normTolerance)) {
      throw new MalformedRotationException(
          "Matrix's kth column is not sufficiently close to unit length.");
    }

    testVal = computeDeterminant(ii, ji, ki, ij, jj, kj, ik, jk, kk);

    if ((testVal < 1.0 - detTolerance) || (testVal > 1.0 + detTolerance)) {
      throw new MalformedRotationException(
          "Matrix's determinant is not sufficiently close to unity.");
    }

  }

  /**
   * Computes the determinant of a three by three matrix described by the supplied components.
   * 
   * @param ii ith row, ith column element
   * @param ji jth row, ith column element
   * @param ki kth row, ith column element
   * @param ij ith row, jth column element
   * @param jj jth row, jth column element
   * @param kj kth row, jth column element
   * @param ik ith row, kth column element
   * @param jk jth row, kth column element
   * @param kk kth row, kth column element
   * 
   * @return the determinant of the matrix described by the supplied components.
   */
  static double computeDeterminant(double ii, double ji, double ki, double ij, double jj, double kj,
      double ik, double jk, double kk) {

    /*
     * TODO: Consider scaling the components by the largest component and then evaluating the
     * determinant function. This may enhance numerical precision.
     */
    return ii * (jj * kk - jk * kj) - ij * (ji * kk - jk * ki) + ik * (ji * kj - jj * ki);
  }

  /**
   * Computes the absolute value of the largest, magnitude, component of a vector expressed in two
   * individual components.
   * 
   * @param i the ith component
   * @param j the jth component
   * 
   * @return the absolute value of the largest, in magnitude, component of the vector [i,j].
   */
  static double absMaxComponent(double i, double j) {
    return max(abs(i), abs(j));
  }

  /**
   * Method that computes the norm of a vector expressed as two separate components. This method is
   * marked with package access to allow other classes in the vector arithmetic toolkit to utilize
   * it.
   * 
   * @param i the ith component
   * @param j the jth component
   * 
   * @return the length of the [i,j] vector
   */
  static double computeNorm(double i, double j) {

    double max = absMaxComponent(i, j);

    /*
     * If max is 0, then vector is clearly the zero vector.
     */
    if (max == 0.0) {
      return 0.0;
    }

    i /= max;
    j /= max;

    /*
     * Since we're trying to avoid overflow in the square root:
     */
    return max * sqrt(i * i + j * j);

  }

  /**
   * Determine if the components of a two by two matrix constitute a rotation.
   * 
   * @param ii ith row, ith column element
   * @param ji jth row, ith column elementt
   * @param ij ith row, jth column element
   * @param jj jth row, jth column element
   * 
   * @param normTolerance tolerance off of unity for the magnitude of the column vectors
   * @param detTolerance tolerance off of unity for the determinant of the matrix
   * 
   * @throws MalformedRotationException if the supplied components do not adequately describe a
   *         rotation given the supplied tolerances
   */
  static void checkRotation(double ii, double ji, double ij, double jj, double normTolerance,
      double detTolerance) throws MalformedRotationException {

    double testVal = computeNorm(ii, ji);
    if ((testVal < 1.0 - normTolerance) || (testVal > 1.0 + normTolerance)) {
      throw new MalformedRotationException(
          "Matrix's ith column is not sufficiently close to unit length.");
    }

    testVal = computeNorm(ij, jj);
    if ((testVal < 1.0 - normTolerance) || (testVal > 1.0 + normTolerance)) {
      throw new MalformedRotationException(
          "Matrix's jth column is not sufficiently close to unit length.");
    }

    testVal = computeDeterminant(ii, ji, ij, jj);

    if ((testVal < 1.0 - detTolerance) || (testVal > 1.0 + detTolerance)) {
      throw new MalformedRotationException(
          "Matrix's determinant is not sufficiently close to unity.");
    }

  }

  /**
   * Computes the determinant of a two by two matrix described by the supplied components.
   * 
   * @param ii ith row, ith column element
   * @param ji jth row, ith column element
   * @param ij ith row, jth column element
   * @param jj jth row, jth column element
   * 
   * @return the determinant of the matrix described by the supplied components.
   */
  static double computeDeterminant(double ii, double ji, double ij, double jj) {

    /*
     * TODO: Consider scaling the components by the largest component and then evaluating the
     * determinant function. This may enhance numerical precision.
     */
    return (ii * jj) - (ji * ij);
  }

}
