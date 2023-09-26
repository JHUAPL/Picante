package picante.math.vectorspace;

import static picante.math.vectorspace.InternalOperations.computeNorm;
import picante.designpatterns.Writable;

/**
 * A writable subclass of the unwritable 3D rotation matrix parent completing another link the
 * weak-immutability design pattern.
 * <p>
 * This class contains the mutator methods necessary to set or alter the internals of the parent
 * classes fields. Wherever these mutations may alter the matrix to become one that no longer
 * qualifies as a rotation, the methods on this class validate the resultant content. This is
 * accomplished through the fact that each setTo method invokes one particular setTo method:
 * {@link RotationMatrixIJK#setTo(double, double, double, double, double, double, double, double, double)}
 * . The validation code is confined to this method, allowing subclasses to override a single method
 * on this class to remove the check code.
 * </p>
 * <p>
 * Similarly each constructor, save the copy constructor and default constructor, validate input
 * content to ensure that it is a rotation matrix. Validation is consistent with the
 * {@link UnwritableMatrixIJK#isRotation()} method.
 * </p>
 */
public class RotationMatrixIJK extends UnwritableRotationMatrixIJK
    implements Writable.ImplementationInterface<UnwritableRotationMatrixIJK, RotationMatrixIJK> {

  /**
   * Instance of a rotation matrix capturing the content of the multiplicative identity.
   */
  public static final UnwritableRotationMatrixIJK IDENTITY =
      new UnwritableRotationMatrixIJK(1, 0, 0, 0, 1, 0, 0, 0, 1);

  /**
   * Creates a rotation matrix and sets it to the identity.
   */
  public RotationMatrixIJK() {
    super(IDENTITY);
  }

  /**
   * Constructs a rotation matrix from the nine basic components.
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
  public RotationMatrixIJK(double ii, double ji, double ki, double ij, double jj, double kj,
      double ik, double jk, double kk) {
    super(ii, ji, ki, ij, jj, kj, ik, jk, kk);
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
  public RotationMatrixIJK(double[][] data) {
    super(data);
  }

  /**
   * Copy constructor, creates a matrix by copying the values of a pre-existing instance of the
   * parent unwritable matrix class.
   * 
   * @param matrix the matrix whose contents are to be copied.
   */
  public RotationMatrixIJK(UnwritableRotationMatrixIJK matrix) {
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
  public RotationMatrixIJK(UnwritableMatrixIJK matrix) {
    super(matrix);
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
  public RotationMatrixIJK(double scale, UnwritableMatrixIJK matrix) {
    super(scale, matrix);
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
  public RotationMatrixIJK(double scaleI, double scaleJ, double scaleK,
      UnwritableMatrixIJK matrix) {
    super(scaleI, scaleJ, scaleK, matrix);
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
  public RotationMatrixIJK(UnwritableVectorIJK ithColumn, UnwritableVectorIJK jthColumn,
      UnwritableVectorIJK kthColumn) {
    super(ithColumn, jthColumn, kthColumn);
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
  public RotationMatrixIJK(double scaleI, UnwritableVectorIJK ithColumn, double scaleJ,
      UnwritableVectorIJK jthColumn, double scaleK, UnwritableVectorIJK kthColumn) {
    super(scaleI, ithColumn, scaleJ, jthColumn, scaleK, kthColumn);
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable rotation subclass, rather
   * than either of the two unwritable parents.
   */
  @Override
  public RotationMatrixIJK createSharpened() {
    return new RotationMatrixIJK(this).sharpen();
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable rotation subclass, rather
   * than either of the two unwritable parents.
   */
  @Override
  public RotationMatrixIJK createTranspose() {
    return new RotationMatrixIJK(this).transpose();
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable rotation subclass, rather
   * than either of the two unwritable parents.
   */
  @Override
  public RotationMatrixIJK createInverse() {
    return createTranspose();
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable rotation subclass, rather
   * than either of the two unwritable parents.
   */
  @Override
  public RotationMatrixIJK createInverse(@SuppressWarnings("unused") double tolerance) {
    return createTranspose();
  }

  /*
   * TODO: Add createInvorted method.
   */

  /**
   * Sharpens the contents of the rotation matrix in place.
   * <p>
   * Sharpening is a process that starts with a rotation matrix and modifies its contents to bring
   * it as close to a rotation as possible given the limits of floating point precision in the
   * implementation. There are many possible rotation matrices that are &quot;sharpenings&quot; of
   * the general rotation matrix. As such, the implementation is unspecified here. The only claims
   * this method makes are that the resultant matrix is as close or closer to a rotation than what
   * you start with.
   * </p>
   * 
   * @return a reference to the instance for convenience.
   */
  public RotationMatrixIJK sharpen() {

    /*
     * Normalize the first column vector of the matrix.
     */
    double norm = computeNorm(ii, ji, ki);
    ii /= norm;
    ji /= norm;
    ki /= norm;

    /*
     * Define the third column of the matrix as the cross product of the first with the second.
     */
    ik = ji * kj - ki * jj;
    jk = ki * ij - ii * kj;
    kk = ii * jj - ji * ij;

    /*
     * Normalize the result.
     */
    norm = computeNorm(ik, jk, kk);
    ik /= norm;
    jk /= norm;
    kk /= norm;

    /*
     * Lastly, cross the third vector with the first to replace the second.
     */
    ij = jk * ki - kk * ji;
    jj = kk * ii - ik * ki;
    kj = ik * ji - jk * ii;

    norm = computeNorm(ij, jj, kj);
    ij /= norm;
    jj /= norm;
    kj /= norm;

    return this;

  }

  /**
   * Transpose the matrix.
   * 
   * @return a reference to the instance for convenience, which now contains the transpose
   */
  public RotationMatrixIJK transpose() {
    double tmp = this.ij;
    this.ij = this.ji;
    this.ji = tmp;

    tmp = this.ik;
    this.ik = this.ki;
    this.ki = tmp;

    tmp = this.jk;
    this.jk = this.kj;
    this.kj = tmp;

    return this;
  }

  /**
   * Sets the components of this matrix to the supplied components
   * <p>
   * <b>Note:</b> Developers wishing to disable all of the "setTo" checking performed by methods on
   * this class need only override this particular method and choose not to invoke the checkRotation
   * method.
   * </p>
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
   * @return a reference to the instance, for convenience, that contains the newly set matrix
   * 
   * @throws IllegalArgumentException if either the columns of the supplied matrix have norms that
   *         are not within {@link UnwritableMatrixIJK#NORM_TOLERANCE} or if the determinant is not
   *         within {@link UnwritableMatrixIJK#DETERMINANT_TOLERANCE}.
   */
  public final RotationMatrixIJK setTo(double ii, double ji, double ki, double ij, double jj,
      double kj, double ik, double jk, double kk) {
    try {
      InternalOperations.checkRotation(ii, ji, ki, ij, jj, kj, ik, jk, kk, NORM_TOLERANCE,
          DETERMINANT_TOLERANCE);
    } catch (MalformedRotationException e) {
      throw new IllegalArgumentException("Matrix components do not describe a rotation.", e);
    }
    this.ii = ii;
    this.ji = ji;
    this.ki = ki;
    this.ij = ij;
    this.jj = jj;
    this.kj = kj;
    this.ik = ik;
    this.jk = jk;
    this.kk = kk;

    return this;
  }

  /**
   * Sets the contents of this matrix to the upper three by three block of a supplied two
   * dimensional array of doubles
   * 
   * @param data the array to copy to the components of this instance
   * 
   * @return a reference to this instance for convenience
   * 
   * @throws IndexOutOfBoundsException if the supplied data array does not contain at least three
   *         arrays of arrays of length three or greater.
   * 
   * @throws IllegalArgumentException if either the columns of the supplied matrix have norms that
   *         are not within {@link UnwritableMatrixIJK#NORM_TOLERANCE} or if the determinant is not
   *         within {@link UnwritableMatrixIJK#DETERMINANT_TOLERANCE}.
   */
  public final RotationMatrixIJK setTo(double[][] data) {
    setTo(data[0][0], data[1][0], data[2][0], data[0][1], data[1][1], data[2][1], data[0][2],
        data[1][2], data[2][2]);
    return this;
  }

  /**
   * Sets the columns of this matrix to the three specified vectors.
   * 
   * @param ithColumn the vector containing the contents to set the ith column
   * @param jthColumn the vector containing the contents to set the jth column
   * @param kthColumn the vector containing the contents to set the kth column
   * 
   * @return a reference to the instance for convenience
   * 
   * @throws IllegalArgumentException if either the columns of the supplied matrix have norms that
   *         are not within {@link UnwritableMatrixIJK#NORM_TOLERANCE} or if the determinant is not
   *         within {@link UnwritableMatrixIJK#DETERMINANT_TOLERANCE}.
   * 
   */
  public final RotationMatrixIJK setTo(UnwritableVectorIJK ithColumn, UnwritableVectorIJK jthColumn,
      UnwritableVectorIJK kthColumn) {
    return setTo(ithColumn.i, ithColumn.j, ithColumn.k, jthColumn.i, jthColumn.j, jthColumn.k,
        kthColumn.i, kthColumn.j, kthColumn.k);
  }

  /**
   * Sets the columns of this matrix to the scaled versions of the supplied vectors.
   * 
   * @param scaleI scale factor to apply to ithColumn
   * @param ithColumn the ith column vector
   * @param scaleJ scale factor to apply to jthColumn
   * @param jthColumn the jth column vector
   * @param scaleK scale factor to apply to kthColumn
   * @param kthColumn the kth column vector
   * 
   * @return a reference to the instance for convenience
   * 
   * @throws IllegalArgumentException if either the columns of the supplied matrix have norms that
   *         are not within {@link UnwritableMatrixIJK#NORM_TOLERANCE} or if the determinant is not
   *         within {@link UnwritableMatrixIJK#DETERMINANT_TOLERANCE}.
   * 
   */
  public final RotationMatrixIJK setTo(double scaleI, UnwritableVectorIJK ithColumn, double scaleJ,
      UnwritableVectorIJK jthColumn, double scaleK, UnwritableVectorIJK kthColumn) {
    return setTo(scaleI * ithColumn.i, scaleI * ithColumn.j, scaleI * ithColumn.k,
        scaleJ * jthColumn.i, scaleJ * jthColumn.j, scaleJ * jthColumn.k, scaleK * kthColumn.i,
        scaleK * kthColumn.j, scaleK * kthColumn.k);
  }

  /**
   * Sets the contents of this rotation matrix to match those of a supplied rotation matrix
   * 
   * @param matrix the matrix to copy
   * 
   * @return a reference to this instance for convenience that contains the supplied components
   */
  @Override
  public final RotationMatrixIJK setTo(UnwritableRotationMatrixIJK matrix) {
    this.ii = matrix.ii;
    this.ji = matrix.ji;
    this.ki = matrix.ki;
    this.ij = matrix.ij;
    this.jj = matrix.jj;
    this.kj = matrix.kj;
    this.ik = matrix.ik;
    this.jk = matrix.jk;
    this.kk = matrix.kk;

    return this;
  }

  /**
   * Sets the contents of this matrix to match those of a supplied matrix
   * 
   * @param matrix the matrix to copy
   * 
   * @return a reference to this instance for convenience that contains the supplied components
   * 
   * @throws IllegalArgumentException if either the columns of the supplied matrix have norms that
   *         are not within {@link UnwritableMatrixIJK#NORM_TOLERANCE} or if the determinant is not
   *         within {@link UnwritableMatrixIJK#DETERMINANT_TOLERANCE}.
   */
  public final RotationMatrixIJK setTo(UnwritableMatrixIJK matrix) {
    return setTo(matrix.ii, matrix.ji, matrix.ki, matrix.ij, matrix.jj, matrix.kj, matrix.ik,
        matrix.jk, matrix.kk);

  }

  /**
   * Sets the contents of this matrix to a sharpened version of a supplied rotation matrix.
   * 
   * @param matrix a rotation matrix to sharpen
   * 
   * @return a reference to the instance, with the contents set to the sharpened version of matrix
   */
  public final RotationMatrixIJK setToSharpened(UnwritableRotationMatrixIJK matrix) {
    setTo(matrix);
    return sharpen();
  }

  /**
   * Sets the contents of this matrix to the transpose of a supplied rotation matrix.
   * 
   * @param matrix a rotation matrix to transpose
   * 
   * @return a reference to the instance, with the contents set to the tranposed version of matrix
   */
  public final RotationMatrixIJK setToTranspose(UnwritableRotationMatrixIJK matrix) {
    setTo(matrix);
    return transpose();
  }

  /**
   * Sets the contents of this matrix to a sharpened version of a supplied matrix.
   * 
   * @param matrix a matrix to sharpen
   * 
   * @return a reference to the instance, with the contents set to the sharpened version of matrix
   * 
   * @throws IllegalArgumentException if either the columns of the supplied matrix have norms that
   *         are not within {@link UnwritableMatrixIJK#NORM_TOLERANCE} or if the determinant is not
   *         within {@link UnwritableMatrixIJK#DETERMINANT_TOLERANCE}.
   */
  public final RotationMatrixIJK setToSharpened(UnwritableMatrixIJK matrix) {
    setTo(matrix);
    return sharpen();
  }

  /**
   * Sets the contents of this matrix to the transpose of a supplied matrix.
   * 
   * @param matrix a matrix to transpose
   * 
   * @return a reference to the instance, with the contents set to the tranposed version of matrix
   * 
   * @throws IllegalArgumentException if either the columns of the supplied matrix have norms that
   *         are not within {@link UnwritableMatrixIJK#NORM_TOLERANCE} or if the determinant is not
   *         within {@link UnwritableMatrixIJK#DETERMINANT_TOLERANCE}.
   */
  public final RotationMatrixIJK setToTranspose(UnwritableMatrixIJK matrix) {
    setTo(matrix);
    return transpose();
  }

  /**
   * Compute the product of a rotation matrix with the transpose of another rotation matrix.
   * 
   * @param a the left hand rotation matrix
   * @param b the right hand rotation matrix to transpose, then multiply
   * 
   * @return a new <code>MatrixIJK</code> containing the resultant product
   * 
   * @see RotationMatrixIJK#mxmt(UnwritableRotationMatrixIJK, UnwritableRotationMatrixIJK,
   *      RotationMatrixIJK)
   */
  public static RotationMatrixIJK mxmt(UnwritableRotationMatrixIJK a,
      UnwritableRotationMatrixIJK b) {
    return mxmt(a, b, new RotationMatrixIJK());
  }

  /**
   * Compute the product of a rotation matrix with the transpose of another rotation matrix.
   * 
   * @param a the left hand rotation matrix
   * @param b the right hand rotation matrix to transpose, then multiply
   * @param buffer the buffer to receive the product, a*transpose(b).
   * 
   * @return a reference to buffer for convenience.
   */
  public static RotationMatrixIJK mxmt(UnwritableRotationMatrixIJK a, UnwritableRotationMatrixIJK b,
      RotationMatrixIJK buffer) {
    double ii = a.ii * b.ii + a.ij * b.ij + a.ik * b.ik;
    double ij = a.ii * b.ji + a.ij * b.jj + a.ik * b.jk;
    double ik = a.ii * b.ki + a.ij * b.kj + a.ik * b.kk;

    double ji = a.ji * b.ii + a.jj * b.ij + a.jk * b.ik;
    double jj = a.ji * b.ji + a.jj * b.jj + a.jk * b.jk;
    double jk = a.ji * b.ki + a.jj * b.kj + a.jk * b.kk;

    double ki = a.ki * b.ii + a.kj * b.ij + a.kk * b.ik;
    double kj = a.ki * b.ji + a.kj * b.jj + a.kk * b.jk;
    double kk = a.ki * b.ki + a.kj * b.kj + a.kk * b.kk;

    buffer.ii = ii;
    buffer.ij = ij;
    buffer.ik = ik;

    buffer.ji = ji;
    buffer.jj = jj;
    buffer.jk = jk;

    buffer.ki = ki;
    buffer.kj = kj;
    buffer.kk = kk;
    return buffer;
  }

  /**
   * Compute the product of a transpose of a rotation matrix with another rotation matrix.
   * 
   * @param a the left hand rotation matrix to transpose, then multiply
   * @param b the right hand rotation matrix
   * 
   * @return a new <code>MatrixIJK</code> containing the product
   * 
   * @see RotationMatrixIJK#mtxm(UnwritableRotationMatrixIJK, UnwritableRotationMatrixIJK,
   *      RotationMatrixIJK)
   */
  public static RotationMatrixIJK mtxm(UnwritableRotationMatrixIJK a,
      UnwritableRotationMatrixIJK b) {
    return mtxm(a, b, new RotationMatrixIJK());
  }

  /**
   * Compute the product of a transpose of a rotation matrix with another rotation matrix.
   * 
   * @param a the left hand rotation matrix to transpose, then multiply
   * @param b the right hand rotation matrix
   * @param buffer the buffer to receive the product, transpose(a)*b.
   * 
   * @return a reference to buffer for convenience
   */
  public static RotationMatrixIJK mtxm(UnwritableRotationMatrixIJK a, UnwritableRotationMatrixIJK b,
      RotationMatrixIJK buffer) {
    double ii = a.ii * b.ii + a.ji * b.ji + a.ki * b.ki;
    double ij = a.ii * b.ij + a.ji * b.jj + a.ki * b.kj;
    double ik = a.ii * b.ik + a.ji * b.jk + a.ki * b.kk;

    double ji = a.ij * b.ii + a.jj * b.ji + a.kj * b.ki;
    double jj = a.ij * b.ij + a.jj * b.jj + a.kj * b.kj;
    double jk = a.ij * b.ik + a.jj * b.jk + a.kj * b.kk;

    double ki = a.ik * b.ii + a.jk * b.ji + a.kk * b.ki;
    double kj = a.ik * b.ij + a.jk * b.jj + a.kk * b.kj;
    double kk = a.ik * b.ik + a.jk * b.jk + a.kk * b.kk;

    buffer.ii = ii;
    buffer.ij = ij;
    buffer.ik = ik;

    buffer.ji = ji;
    buffer.jj = jj;
    buffer.jk = jk;

    buffer.ki = ki;
    buffer.kj = kj;
    buffer.kk = kk;
    return buffer;
  }

  /**
   * Compute the product of two rotation matrices.
   * 
   * @param a the left hand rotation matrix
   * @param b the right hand rotation matrix
   * 
   * @return a new <code>RotationMatrixIJK</code> containing the product (ab)
   * 
   * @see RotationMatrixIJK#mxm(UnwritableRotationMatrixIJK, UnwritableRotationMatrixIJK,
   *      RotationMatrixIJK)
   */
  public static RotationMatrixIJK mxm(UnwritableRotationMatrixIJK a,
      UnwritableRotationMatrixIJK b) {
    return mxm(a, b, new RotationMatrixIJK());
  }

  /**
   * Compute the product of two rotation matrices.
   * 
   * @param a the left hand rotation matrix
   * @param b the right hand rotation matrix
   * @param buffer the buffer to receive the product, a*b.
   * 
   * @return a reference to buffer for convenience
   */
  public static RotationMatrixIJK mxm(UnwritableRotationMatrixIJK a, UnwritableRotationMatrixIJK b,
      RotationMatrixIJK buffer) {
    double ii = a.ii * b.ii + a.ij * b.ji + a.ik * b.ki;
    double ij = a.ii * b.ij + a.ij * b.jj + a.ik * b.kj;
    double ik = a.ii * b.ik + a.ij * b.jk + a.ik * b.kk;

    double ji = a.ji * b.ii + a.jj * b.ji + a.jk * b.ki;
    double jj = a.ji * b.ij + a.jj * b.jj + a.jk * b.kj;
    double jk = a.ji * b.ik + a.jj * b.jk + a.jk * b.kk;

    double ki = a.ki * b.ii + a.kj * b.ji + a.kk * b.ki;
    double kj = a.ki * b.ij + a.kj * b.jj + a.kk * b.kj;
    double kk = a.ki * b.ik + a.kj * b.jk + a.kk * b.kk;

    buffer.ii = ii;
    buffer.ij = ij;
    buffer.ik = ik;

    buffer.ji = ji;
    buffer.jj = jj;
    buffer.jk = jk;

    buffer.ki = ki;
    buffer.kj = kj;
    buffer.kk = kk;
    return buffer;
  }

  /**
   * Creates a sharpened matrix from the supplied inputs without performing any checks on the
   * inputs.
   * 
   * @param ii
   * @param ji
   * @param ki
   * @param ij
   * @param jj
   * @param kj
   * @param ik
   * @param jk
   * @param kk
   * 
   * @return
   * 
   * @throws IllegalArgumentException if, after sharpening, the resultant matrix is still not a
   *         rotation.
   */
  public static RotationMatrixIJK createSharpened(double ii, double ji, double ki, double ij,
      double jj, double kj, double ik, double jk, double kk) {

    /*
     * This is necessary to be able to leave the variable names as would be expected on this class,
     * since they are shadowed by the fields on the anonymous inner class used to subvert the
     * rotation check.
     */
    final double aii = ii;
    final double aji = ji;
    final double aki = ki;
    final double aij = ij;
    final double ajj = jj;
    final double akj = kj;
    final double aik = ik;
    final double ajk = jk;
    final double akk = kk;

    /*
     * By pass the constructor check, and execute sharpen directly.
     */
    RotationMatrixIJK source = new RotationMatrixIJK() {
      {
        this.ii = aii;
        this.ji = aji;
        this.ki = aki;
        this.ij = aij;
        this.jj = ajj;
        this.kj = akj;
        this.ik = aik;
        this.jk = ajk;
        this.kk = akk;
      }
    };

    /*
     * Check the determinant of source to see that it is at least postiive.
     */
    if (source.getDeterminant() <= 0) {
      throw new IllegalArgumentException(
          "Source has a determinant that is not strictly positive.  Unable to sharpen source into a rotation matrix.");
    }

    /*
     * Return an actual RotationMatrixIJK, not the anonymous subclass.
     */
    source.sharpen();
    return new RotationMatrixIJK(source.ii, source.ji, source.ki, source.ij, source.jj, source.kj,
        source.ik, source.jk, source.kk);
  }

}
