package picante.math.vectorspace;

import static picante.math.vectorspace.InternalOperations.computeNorm;

/**
 * A weakly immutable extension of the unwritable matrix class designed to add rotation matrix
 * specific functionality.
 * <p>
 * <b>Note:</b> The constructors of this class that take arguments that may specify matrices which
 * are not rotations, validate the supplied input to ensure that it is sufficiently close to a
 * rotation. If it passes the check, the content are <b>not</b> modified to bring it closer to a
 * rotation. See {@link #createSharpened()} for details on how to do this. The check invoked by this
 * routine is consistent with {@link UnwritableMatrixIJK#isRotation()}. If you do not desire this
 * particular behavior, then simply subclass this class or the provided writable subclass to suit
 * your needs.
 * </p>
 * <p>
 * Another point worth making about this class and its writable subclass, they derive their
 * definition of equals and hashcode from the top level unwritable matrix class. The consequence of
 * this is that only the components of the actual matrix itself are used in the comparision. As
 * such, it is possible to construct a matrix that is equivalent to a rotation matrix and vice
 * versa. It was decided at the time these classes were created that this would be the appropriate
 * default or desired behavior. If you have a specific need to treat matrices that have equal
 * components but live in different class representations in this inheritance hierarchy, then it
 * would be best to place them in containers and define equality on the containers. In practice,
 * this should not be necessary.
 * </p>
 */
public class UnwritableRotationMatrixIJK extends UnwritableMatrixIJK {

  /**
   * Method that validate supplied input content is sufficiently close to a rotation matrix.
   * 
   * @param matrix the matrix content from the parent class to validate
   * 
   * @throws MalformedRotationException if either the columns of the supplied matrix have norms that
   *         are not within {@link UnwritableMatrixIJK#NORM_TOLERANCE} or if the determinant is not
   *         within {@link UnwritableMatrixIJK#DETERMINANT_TOLERANCE}.
   */
  protected static void checkRotation(UnwritableMatrixIJK matrix)
      throws MalformedRotationException {
    InternalOperations.checkRotation(matrix.ii, matrix.ji, matrix.ki, matrix.ij, matrix.jj,
        matrix.kj, matrix.ik, matrix.jk, matrix.kk, NORM_TOLERANCE, DETERMINANT_TOLERANCE);
  }

  /**
   * Protected no argument, no operation constructor for subclasses to utilize.
   */
  protected UnwritableRotationMatrixIJK() {
    super();
  }

  /**
   * Constructs a rotation matrix from the supplied double values.
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
   * @throws IllegalArgumentException if either the columns of the supplied matrix have norms that
   *         are not within {@link UnwritableMatrixIJK#NORM_TOLERANCE} or if the determinant is not
   *         within {@link UnwritableMatrixIJK#DETERMINANT_TOLERANCE}.
   * 
   */
  public UnwritableRotationMatrixIJK(double ii, double ji, double ki, double ij, double jj,
      double kj, double ik, double jk, double kk) {
    super(ii, ji, ki, ij, jj, kj, ik, jk, kk);
    try {
      checkRotation(this);
    } catch (MalformedRotationException e) {
      throw new IllegalArgumentException("Matrix components do not describe a rotation.", e);
    }
  }

  /**
   * Constructs a matrix from the upper three by three block of a two dimensional array of doubles.
   * 
   * <p>
   * The values from the data array are copied into the matrix as follows: <br>
   * <table>
   * <tr>
   * <td>data[0][0]</td>
   * <td>data[0][1]</td>
   * <td>data[0][2]</td>
   * </tr>
   * <tr>
   * <td>data[1][0]</td>
   * <td>data[1][1]</td>
   * <td>data[1][2]</td>
   * </tr>
   * <tr>
   * <td>data[2][0]</td>
   * <td>data[2][1]</td>
   * <td>data[2][2]</td>
   * </tr>
   * </table>
   * </p>
   * 
   * @param data the array of doubles
   * 
   * @throws IndexOutOfBoundsException if the supplied data array does not contain at least three
   *         arrays of arrays of length three or greater.
   * @throws IllegalArgumentException if either the columns of the supplied matrix have norms that
   *         are not within {@link UnwritableMatrixIJK#NORM_TOLERANCE} or if the determinant is not
   *         within {@link UnwritableMatrixIJK#DETERMINANT_TOLERANCE}.
   */
  public UnwritableRotationMatrixIJK(double[][] data) {
    this(data[0][0], data[1][0], data[2][0], data[0][1], data[1][1], data[2][1], data[0][2],
        data[1][2], data[2][2]);
  }

  /**
   * Copy constructor, creates a matrix by copying the values of a pre-existing instance of the
   * parent unwritable matrix class.
   * 
   * @param matrix the matrix whose contents are to be copied.
   */
  public UnwritableRotationMatrixIJK(UnwritableRotationMatrixIJK matrix) {
    super(matrix);
  }

  /**
   * Copy constructor, of sorts, creates a matrix by copying the values of a pre-existing matrix.
   * <p>
   * <b>Note:</b>This constructor performs no validation on the input by design.
   * </p>
   * 
   * @param matrix the rotation matrix whose contents are to be copied.
   * 
   * @throws IllegalArgumentException if either the columns of the supplied matrix have norms that
   *         are not within {@link UnwritableMatrixIJK#NORM_TOLERANCE} or if the determinant is not
   *         within {@link UnwritableMatrixIJK#DETERMINANT_TOLERANCE}.
   */
  public UnwritableRotationMatrixIJK(UnwritableMatrixIJK matrix) {
    this(matrix.ii, matrix.ji, matrix.ki, matrix.ij, matrix.jj, matrix.kj, matrix.ik, matrix.jk,
        matrix.kk);
  }

  /**
   * Scaling constructor, creates a new matrix by applying a scalar multiple to the components of a
   * pre-existing matrix.
   * 
   * @param scale the scale factor to apply
   * @param matrix the matrix whose components are to be scaled and copied.
   * 
   * @throws IllegalArgumentException if either the columns of the supplied matrix have norms that
   *         are not within {@link UnwritableMatrixIJK#NORM_TOLERANCE} or if the determinant is not
   *         within {@link UnwritableMatrixIJK#DETERMINANT_TOLERANCE}.
   */
  public UnwritableRotationMatrixIJK(double scale, UnwritableMatrixIJK matrix) {
    this(scale * matrix.ii, scale * matrix.ji, scale * matrix.ki, scale * matrix.ij,
        scale * matrix.jj, scale * matrix.kj, scale * matrix.ik, scale * matrix.jk,
        scale * matrix.kk);
  }

  /**
   * Column scaling constructor, creates a new rotation matrix by applying scalar multiples to the
   * columns of a pre-existing matrix.
   * 
   * @param scaleI scale factor to apply to the ith column
   * @param scaleJ scale factor to apply to the jth column
   * @param scaleK scale factor to apply to the kth column
   * @param matrix the matrix whose components are to be scaled and copied
   * 
   * @throws IllegalArgumentException if either the columns of the supplied matrix have norms that
   *         are not within {@link UnwritableMatrixIJK#NORM_TOLERANCE} or if the determinant is not
   *         within {@link UnwritableMatrixIJK#DETERMINANT_TOLERANCE}.
   */
  public UnwritableRotationMatrixIJK(double scaleI, double scaleJ, double scaleK,
      UnwritableMatrixIJK matrix) {
    this(scaleI * matrix.ii, scaleI * matrix.ji, scaleI * matrix.ki, scaleJ * matrix.ij,
        scaleJ * matrix.jj, scaleJ * matrix.kj, scaleK * matrix.ik, scaleK * matrix.jk,
        scaleK * matrix.kk);
  }

  /**
   * Column vector constructor, creates a new matrix by populating the columns of the rotation
   * matrix with the supplied vectors.
   * 
   * @param ithColumn the vector containing the ith column
   * @param jthColumn the vector containing the jth column
   * @param kthColumn the vector containing the kth column
   * 
   * @throws IllegalArgumentException if either the columns of the supplied matrix have norms that
   *         are not within {@link UnwritableMatrixIJK#NORM_TOLERANCE} or if the determinant is not
   *         within {@link UnwritableMatrixIJK#DETERMINANT_TOLERANCE}.
   */
  public UnwritableRotationMatrixIJK(UnwritableVectorIJK ithColumn, UnwritableVectorIJK jthColumn,
      UnwritableVectorIJK kthColumn) {
    this(ithColumn.i, ithColumn.j, ithColumn.k, jthColumn.i, jthColumn.j, jthColumn.k, kthColumn.i,
        kthColumn.j, kthColumn.k);
  }

  /**
   * Scaled column vector constructor, creates a new rotation matrix by populating the columns of
   * the matrix with scaled versions of the supplied vectors
   * 
   * @param scaleI the scale factor to apply to the ith column
   * @param ithColumn the vector containing the ith column
   * @param scaleJ the scale factor to apply to the jth column
   * @param jthColumn the vector containing the jth column
   * @param scaleK the scale factor to apply to the kth column
   * @param kthColumn the vector containing the kth column
   * 
   * @throws IllegalArgumentException if either the columns of the supplied matrix have norms that
   *         are not within {@link UnwritableMatrixIJK#NORM_TOLERANCE} or if the determinant is not
   *         within {@link UnwritableMatrixIJK#DETERMINANT_TOLERANCE}.
   */
  public UnwritableRotationMatrixIJK(double scaleI, UnwritableVectorIJK ithColumn, double scaleJ,
      UnwritableVectorIJK jthColumn, double scaleK, UnwritableVectorIJK kthColumn) {
    this(scaleI * ithColumn.i, scaleI * ithColumn.j, scaleI * ithColumn.k, scaleJ * jthColumn.i,
        scaleJ * jthColumn.j, scaleJ * jthColumn.k, scaleK * kthColumn.i, scaleK * kthColumn.j,
        scaleK * kthColumn.k);
  }

  /**
   * Creates a new, sharpened copy of the existing rotation matrix.
   * <p>
   * Sharpening is a process that starts with a rotation matrix and modifies its contents to bring
   * it as close to a rotation as possible given the limits of floating point precision in the
   * implementation. There are many possible rotation matrices that are &quot;sharpenings&quot; of
   * the general rotation matrix. As such, the implementation is unspecified here. The only claims
   * this method makes are that the resultant matrix is as close or closer to a rotation than what
   * you start with.
   * </p>
   * 
   * @return the sharpened version of the instance.
   */
  public UnwritableRotationMatrixIJK createSharpened() {

    UnwritableRotationMatrixIJK result = new UnwritableRotationMatrixIJK(this);

    /*
     * Normalize the first column vector of the matrix.
     */
    double norm = computeNorm(result.ii, result.ji, result.ki);
    result.ii /= norm;
    result.ji /= norm;
    result.ki /= norm;

    /*
     * Define the third column of the matrix as the cross product of the first with the second.
     */
    result.ik = result.ji * result.kj - result.ki * result.jj;
    result.jk = result.ki * result.ij - result.ii * result.kj;
    result.kk = result.ii * result.jj - result.ji * result.ij;

    /*
     * Normalize the result.
     */
    norm = computeNorm(result.ik, result.jk, result.kk);
    result.ik /= norm;
    result.jk /= norm;
    result.kk /= norm;

    /*
     * Lastly, cross the third vector with the first to replace the second.
     */
    result.ij = result.jk * result.ki - result.kk * result.ji;
    result.jj = result.kk * result.ii - result.ik * result.ki;
    result.kj = result.ik * result.ji - result.jk * result.ii;

    norm = computeNorm(result.ij, result.jj, result.kj);
    result.ij /= norm;
    result.jj /= norm;
    result.kj /= norm;

    return result;
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the unwritable rotation subclass
   * rather than the unwritable plain matrix parent.
   */
  @Override
  public UnwritableRotationMatrixIJK createTranspose() {
    return new UnwritableRotationMatrixIJK(this.ii, this.ij, this.ik, this.ji, this.jj, this.jk,
        this.ki, this.kj, this.kk);
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the unwritable rotation subclass
   * rather than the unwritable plain matrix parent.
   */
  @Override
  public UnwritableRotationMatrixIJK createInverse() {
    /*
     * Matrix inversion in the special case of rotation matrices is transposition.
     */
    return createTranspose();
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the unwritable rotation subclass
   * rather than the unwritable plain matrix parent.
   */
  @Override
  public UnwritableRotationMatrixIJK createInverse(@SuppressWarnings("unused") double tolerance) {
    return createTranspose();
  }

  /**
   * Makes an unwritable copy of the supplied rotation matrix.
   * <p>
   * This method makes an unwritable copy only if necessary. It tries to avoid making a copy
   * wherever possible.
   * </p>
   * 
   * @param matrix an matrix to copy.
   * 
   * @return either a reference to matrix (if matrix is already only an instance of
   *         {@link UnwritableMatrixIJK}, otherwise an unwritable copy of matrix's contents
   */
  public static UnwritableRotationMatrixIJK copyOf(UnwritableRotationMatrixIJK matrix) {
    if (matrix.getClass().equals(UnwritableRotationMatrixIJK.class)) {
      return matrix;
    }
    return new UnwritableRotationMatrixIJK(matrix);
  }
}
