package picante.math.vectorspace;

import static picante.math.PicanteMath.abs;
import static picante.math.vectorspace.InternalOperations.computeDeterminant;
import static picante.math.vectorspace.InternalOperations.computeNorm;

/**
 * A weakly immutable 3-dimensional matrix designed to properly support several writable subclasses.
 * <p>
 * <b>Note:</b>Subclass implementers, you should only use the protected fields in this class to
 * store the contents of the matrix components, otherwise all of the methods here and in the
 * operations class may break.
 * </p>
 * <p>
 * The basic data fields on this class are marked as protected to allow direct access to them
 * through subclassing. This will get around any performance issues that one may have in utilizing
 * this matrix arithmetic toolkit due to the enforcement of access to the component values through
 * accessor methods.
 * </p>
 * <p>
 * Note, the equals and hashcode implementations in this class support proper comparisons between
 * subclasses of this class and this class. The reason this works is because by design the only
 * member variables of this class live in the parent class. If one subclasses this class and defines
 * additional members then this will most certainly break the implementation presented here.
 * </p>
 * <p>
 * The protected fields in this matrix are arranged in the following manner: <br>
 * <table>
 * <tr>
 * <td>ii</td>
 * <td>ij</td>
 * <td>ik</td>
 * </tr>
 * <tr>
 * <td>ji</td>
 * <td>jj</td>
 * <td>jk</td>
 * </tr>
 * <tr>
 * <td>ki</td>
 * <td>kj</td>
 * <td>kk</td>
 * </tr>
 * </table>
 * <br>
 * The class prefers column ordering, so when working with vectors and this class they are to be
 * considered &quot;column&quot; vectors.
 * </p>
 */
public class UnwritableMatrixIJK {

  /**
   * Default tolerance for determining if a matrix is invertible. The determinant must be greater
   * than this tolerance: {@value #INVERSION_TOLERANCE}
   */
  public static final double INVERSION_TOLERANCE = 1E-16;

  /**
   * One of the two default tolerances that control how close to a rotation a rotation matrix must
   * be. This value determines how far off unity the norm of the column vectors of a matrix must be.
   * Currently it is set to: {@value #NORM_TOLERANCE}
   */
  public static final double NORM_TOLERANCE = 1E-4;

  /**
   * The other of the two default tolerances that control how close to a rotation a rotation matrix
   * must be. This value determines how far off unity the determinant of the matrix must be.
   * Currently it is set to: {@value #DETERMINANT_TOLERANCE}
   */
  public static final double DETERMINANT_TOLERANCE = 1E-4;

  /**
   * The bound defining the boundary length at which the invort procedure works with double
   * precision. Note: this is necessary because larger negative exponents are captured by 64 IEEE
   * doubles than positive ones.
   */
  public static final double INVORSION_BOUND = Double.MAX_VALUE;

  /**
   * The ith row, ith column component of the matrix.
   */
  protected double ii;

  /**
   * The ith row, jth column component of the matrix.
   */
  protected double ij;

  /**
   * The ith row, kth column component of the matrix.
   */
  protected double ik;

  /**
   * The jth row, ith column component of the matrix.
   */
  protected double ji;

  /**
   * The jth row, jth column component of the matrix.
   */
  protected double jj;

  /**
   * The jth row, kth column component of the matrix.
   */
  protected double jk;

  /**
   * The kth row, ith column component of the matrix.
   */
  protected double ki;

  /**
   * The kth row, jth column component of the matrix.
   */
  protected double kj;

  /**
   * The kth row, kth column component of the matrix.
   */
  protected double kk;

  /**
   * Protected, no argument, no initialization constructor for subclasses to utilize.
   */
  protected UnwritableMatrixIJK() {
    super();
  }

  /**
   * Constructs a matrix from the nine basic components.
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
   */
  public UnwritableMatrixIJK(double ii, double ji, double ki, double ij, double jj, double kj,
      double ik, double jk, double kk) {
    super();
    this.ii = ii;
    this.ji = ji;
    this.ki = ki;
    this.ij = ij;
    this.jj = jj;
    this.kj = kj;
    this.ik = ik;
    this.jk = jk;
    this.kk = kk;
  }

  /**
   * Constructs a matrix from the upper three by three block of a two dimensional array of doubles.
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
   */
  public UnwritableMatrixIJK(double[][] data) {
    this(data[0][0], data[1][0], data[2][0], data[0][1], data[1][1], data[2][1], data[0][2],
        data[1][2], data[2][2]);
  }

  /**
   * Copy constructor, creates a matrix by copying the values of a pre-existing one.
   * 
   * @param matrix the matrix whose contents are to be copied.
   */
  public UnwritableMatrixIJK(UnwritableMatrixIJK matrix) {
    this(matrix.ii, matrix.ji, matrix.ki, matrix.ij, matrix.jj, matrix.kj, matrix.ik, matrix.jk,
        matrix.kk);
  }

  /**
   * Scaling constructor, creates a new matrix by applying a scalar multiple to the components of a
   * pre-existing matrix.
   * 
   * @param scale the scale factor to apply
   * @param matrix the matrix whose components are to be scaled and copied
   */
  public UnwritableMatrixIJK(double scale, UnwritableMatrixIJK matrix) {
    this(scale * matrix.ii, scale * matrix.ji, scale * matrix.ki, scale * matrix.ij,
        scale * matrix.jj, scale * matrix.kj, scale * matrix.ik, scale * matrix.jk,
        scale * matrix.kk);
  }

  /**
   * Column scaling constructor, creates a new matrix by applying scalar multiples to the columns of
   * a pre-existing matrix.
   * 
   * @param scaleI scale factor to apply to the ith column
   * @param scaleJ scale factor to apply to the jth column
   * @param scaleK scale factor to apply to the kth column
   * @param matrix the matrix whose components are to be scaled and copied
   */
  public UnwritableMatrixIJK(double scaleI, double scaleJ, double scaleK,
      UnwritableMatrixIJK matrix) {
    this(scaleI * matrix.ii, scaleI * matrix.ji, scaleI * matrix.ki, scaleJ * matrix.ij,
        scaleJ * matrix.jj, scaleJ * matrix.kj, scaleK * matrix.ik, scaleK * matrix.jk,
        scaleK * matrix.kk);
  }

  /**
   * Column vector constructor, creates a new matrix by populating the columns of the matrix with
   * the supplied vectors.
   * 
   * @param ithColumn the vector containing the ith column
   * @param jthColumn the vector containing the jth column
   * @param kthColumn the vector containing the kth column
   */
  public UnwritableMatrixIJK(UnwritableVectorIJK ithColumn, UnwritableVectorIJK jthColumn,
      UnwritableVectorIJK kthColumn) {
    this(ithColumn.i, ithColumn.j, ithColumn.k, jthColumn.i, jthColumn.j, jthColumn.k, kthColumn.i,
        kthColumn.j, kthColumn.k);
  }

  /**
   * Scaled column vector constructor, creates a new matrix by populating the columns of the matrix
   * with scaled versions of the supplied vectors
   * 
   * @param scaleI the scale factor to apply to the ith column
   * @param ithColumn the vector containing the ith column
   * @param scaleJ the scale factor to apply to the jth column
   * @param jthColumn the vector containing the jth column
   * @param scaleK the scale factor to apply to the kth column
   * @param kthColumn the vector containing the kth column
   */
  public UnwritableMatrixIJK(double scaleI, UnwritableVectorIJK ithColumn, double scaleJ,
      UnwritableVectorIJK jthColumn, double scaleK, UnwritableVectorIJK kthColumn) {
    this(scaleI * ithColumn.i, scaleI * ithColumn.j, scaleI * ithColumn.k, scaleJ * jthColumn.i,
        scaleJ * jthColumn.j, scaleJ * jthColumn.k, scaleK * kthColumn.i, scaleK * kthColumn.j,
        scaleK * kthColumn.k);

  }

  /**
   * Creates a new, transposed copy of the existing matrix.
   * 
   * @return the transpose of the instance
   */
  public UnwritableMatrixIJK createTranspose() {
    return new UnwritableMatrixIJK(this.ii, this.ij, this.ik, this.ji, this.jj, this.jk, this.ki,
        this.kj, this.kk);
  }

  /**
   * Creates a new matrix whose columns are unitized versions of the columns of this matrix.
   * 
   * @return the unitized column version of this matrix
   * 
   * @throws UnsupportedOperationException if any of the columns are of length zero
   */
  public UnwritableMatrixIJK createUnitizedColumns() {
    return new UnwritableMatrixIJK(new VectorIJK(this.ii, this.ji, this.ki).unitize(), // first
        // column
        new VectorIJK(this.ij, this.jj, this.kj).unitize(), // second
        // column
        new VectorIJK(this.ik, this.jk, this.kk).unitize()); // third
    // column
  }

  /**
   * Creates the adjugate (i.e., the transpose of the cofactor matrix) of this matrix. Dividing the
   * resultant matrix by the determinant of this matrix (assuming it is non-zero) yields the inverse
   * of this matrix.
   * 
   * @return the adjugate of the instance
   */
  public UnwritableMatrixIJK createAdjugate() {
    return new UnwritableMatrixIJK((jj * kk - kj * jk), -(ji * kk - ki * jk), (ji * kj - ki * jj),
        -(ij * kk - kj * ik), (ii * kk - ki * ik), -(ii * kj - ki * ij), (ij * jk - jj * ik),
        -(ii * jk - ji * ik), (ii * jj - ji * ij));
  }

  /**
   * Creates a new, inverted copy of the existing matrix if possible.
   * 
   * @return the multiplicative inverse of the instance
   * 
   * @throws UnsupportedOperationException if the instance matrix has a determinant within
   *         {@value #INVERSION_TOLERANCE} of 0.0
   */
  public UnwritableMatrixIJK createInverse() {
    return createInverse(INVERSION_TOLERANCE);
  }

  /**
   * Creates a new, inverted copy of the existing matrix.
   * 
   * @param tolerance the absolute value of the determinant of the instance must be greater than
   *        this for inversion to proceed
   * 
   * @return the multiplicative inverse of the instance
   * 
   * @throws UnsupportedOperationException if the instance matrix has a determinant within tolerance
   *         of 0.0
   */
  public UnwritableMatrixIJK createInverse(double tolerance) {
    double det = getDeterminant();

    if (abs(det) < tolerance) {
      throw new UnsupportedOperationException("Matrix nearly singular, unable to invert.");
    }

    return new UnwritableMatrixIJK((jj * kk - kj * jk) / det, -(ji * kk - ki * jk) / det,
        (ji * kj - ki * jj) / det, -(ij * kk - kj * ik) / det, (ii * kk - ki * ik) / det,
        -(ii * kj - ki * ij) / det, (ij * jk - jj * ik) / det, -(ii * jk - ji * ik) / det,
        (ii * jj - ji * ij) / det);

  }

  /**
   * Creates a new, inverted copy of the existing matrix with orthogonal columns.
   * <p>
   * If this method is invoked on matrices whose columns are not orthogonal, the resultant matrix is
   * likely not the inverse sought. Use the more general {@link UnwritableMatrixIJK#createInverse()}
   * method instead.
   * </p>
   * 
   * @return a newly created matrix, that is the inverse of this matrix if it meets the
   *         orthogonality condition
   * 
   * @throws UnsupportedOperationException if the lengths of any of the columns are zero or too
   *         small to properly invert multiplicatively in the space available to double precision.
   */
  public UnwritableMatrixIJK createInvorted() {

    /*
     * First create the transpose, then all that's left is to scale the rows appropriately.
     */
    UnwritableMatrixIJK matrix = this.createTranspose();

    double length = computeNorm(matrix.ii, matrix.ij, matrix.ik);

    if ((length * INVORSION_BOUND < 1) || (length == 0)) {
      throw new UnsupportedOperationException(
          "ith column of matrix has length, " + length + ", for which there is no inverse.");
    }

    matrix.ii /= length;
    matrix.ii /= length;
    matrix.ij /= length;
    matrix.ij /= length;
    matrix.ik /= length;
    matrix.ik /= length;

    length = computeNorm(matrix.ji, matrix.jj, matrix.jk);

    if ((length * INVORSION_BOUND < 1) || (length == 0)) {
      throw new UnsupportedOperationException(
          "jth column of matrix has length, " + length + ", for which there is no inverse.");
    }

    matrix.ji /= length;
    matrix.ji /= length;
    matrix.jj /= length;
    matrix.jj /= length;
    matrix.jk /= length;
    matrix.jk /= length;

    length = computeNorm(matrix.ki, matrix.kj, matrix.kk);

    if ((length * INVORSION_BOUND < 1) || (length == 0)) {
      throw new UnsupportedOperationException(
          "kth column of matrix has length, " + length + ", for which there is no inverse.");
    }

    matrix.ki /= length;
    matrix.ki /= length;
    matrix.kj /= length;
    matrix.kj /= length;
    matrix.kk /= length;
    matrix.kk /= length;

    return matrix;
  }

  /**
   * Gets the ith row, ith column component.
   * <p>
   * <br>
   * <table>
   * <tr>
   * <td><b>ii<b></td>
   * <td>ij</td>
   * <td>ik</td>
   * </tr>
   * <tr>
   * <td>ji</td>
   * <td>jj</td>
   * <td>jk</td>
   * </tr>
   * <tr>
   * <td>ki</td>
   * <td>kj</td>
   * <td>kk</td>
   * </tr>
   * </table>
   * <br>
   * </p>
   * 
   * @return the ith row, ith column value
   */
  public final double getII() {
    return ii;
  }

  /**
   * Gets the jth row, ith column component.
   * <p>
   * <br>
   * <table>
   * <tr>
   * <td>ii</td>
   * <td>ij</td>
   * <td>ik</td>
   * </tr>
   * <tr>
   * <td><b>ji<b></td>
   * <td>jj</td>
   * <td>jk</td>
   * </tr>
   * <tr>
   * <td>ki</td>
   * <td>kj</td>
   * <td>kk</td>
   * </tr>
   * </table>
   * <br>
   * </p>
   * 
   * @return the jth row, ith column value
   */
  public final double getJI() {
    return ji;
  }

  /**
   * Gets the kth row, ith column component.
   * <p>
   * <br>
   * <table>
   * <tr>
   * <td>ii</td>
   * <td>ij</td>
   * <td>ik</td>
   * </tr>
   * <tr>
   * <td>ji</td>
   * <td>jj</td>
   * <td>jk</td>
   * </tr>
   * <tr>
   * <td><b>ki</b></td>
   * <td>kj</td>
   * <td>kk</td>
   * </tr>
   * </table>
   * <br>
   * </p>
   * 
   * @return the kth row, ith column value
   */
  public final double getKI() {
    return ki;
  }

  /**
   * Gets the ith row, jth column component.
   * <p>
   * <br>
   * <table>
   * <tr>
   * <td>ii</td>
   * <td><b>ij</b></td>
   * <td>ik</td>
   * </tr>
   * <tr>
   * <td>ji</td>
   * <td>jj</td>
   * <td>jk</td>
   * </tr>
   * <tr>
   * <td>ki</td>
   * <td>kj</td>
   * <td>kk</td>
   * </tr>
   * </table>
   * <br>
   * </p>
   * 
   * 
   * @return the ith row, jth column value
   */
  public final double getIJ() {
    return ij;
  }

  /**
   * Gets the jth row, jth column component.
   * <p>
   * <br>
   * <table>
   * <tr>
   * <td>ii</td>
   * <td>ij</td>
   * <td>ik</td>
   * </tr>
   * <tr>
   * <td>ji</td>
   * <td><b>jj</b></td>
   * <td>jk</td>
   * </tr>
   * <tr>
   * <td>ki</td>
   * <td>kj</td>
   * <td>kk</td>
   * </tr>
   * </table>
   * <br>
   * </p>
   * 
   * @return the jth row, jth column value
   */
  public final double getJJ() {
    return jj;
  }

  /**
   * Gets the kth row, jth column component.
   * <p>
   * <br>
   * <table>
   * <tr>
   * <td>ii</td>
   * <td>ij</td>
   * <td>ik</td>
   * </tr>
   * <tr>
   * <td>ji</td>
   * <td>jj</td>
   * <td>jk</td>
   * </tr>
   * <tr>
   * <td>ki</td>
   * <td><b>kj</b></td>
   * <td>kk</td>
   * </tr>
   * </table>
   * <br>
   * </p>
   * 
   * @return the kth row, jth column value
   */
  public final double getKJ() {
    return kj;
  }

  /**
   * Gets the ith row, kth column component.
   * <p>
   * <br>
   * <table>
   * <tr>
   * <td>ii</td>
   * <td>ij</td>
   * <td><b>ik</b></td>
   * </tr>
   * <tr>
   * <td>ji</td>
   * <td>jj</td>
   * <td>jk</td>
   * </tr>
   * <tr>
   * <td>ki</td>
   * <td>kj</td>
   * <td>kk</td>
   * </tr>
   * </table>
   * <br>
   * </p>
   * 
   * @return the ith row, kth column value
   */
  public final double getIK() {
    return ik;
  }

  /**
   * Gets the jth row, kth column value.
   * <p>
   * <br>
   * <table>
   * <tr>
   * <td>ii</td>
   * <td>ij</td>
   * <td>ik</td>
   * </tr>
   * <tr>
   * <td>ji</td>
   * <td>jj</td>
   * <td><b>jk</b></td>
   * </tr>
   * <tr>
   * <td>ki</td>
   * <td>kj</td>
   * <td>kk</td>
   * </tr>
   * </table>
   * <br>
   * </p>
   * 
   * @return the jth row, kth column value.
   */
  public final double getJK() {
    return jk;
  }

  /**
   * Gets the kth row, kth column component.
   * <p>
   * <br>
   * <table>
   * <tr>
   * <td>ii</td>
   * <td>ij</td>
   * <td>ik</td>
   * </tr>
   * <tr>
   * <td>ji</td>
   * <td>jj</td>
   * <td>jk</td>
   * </tr>
   * <tr>
   * <td>ki</td>
   * <td>kj</td>
   * <td><b>kk</b></td>
   * </tr>
   * </table>
   * <br>
   * </p>
   * 
   * @return kth row, kth column value
   */
  public final double getKK() {
    return kk;
  }

  /**
   * Gets the component from the specified row and column.
   * 
   * <p>
   * <br>
   * <table>
   * <tr>
   * <td>ii (0,0)</td>
   * <td>ij (0,1)</td>
   * <td>ik (0,2)</td>
   * </tr>
   * <tr>
   * <td>ji (1,0)</td>
   * <td>jj (1,1)</td>
   * <td>jk (1,2)</td>
   * </tr>
   * <tr>
   * <td>ki (2,0)</td>
   * <td>kj (2,1)</td>
   * <td>kk (2,2)</td>
   * </tr>
   * </table>
   * <br>
   * </p>
   * 
   * @param row a row index in [0,2].
   * @param column a column index in [0,2]
   * 
   * @return the desired matrix component value
   * 
   * @throws IllegalArgumentException if either the supplied row or column index are outside their
   *         acceptable ranges of [0,2].
   */
  public final double get(int row, int column) {

    switch (row) {
      case 0:
        switch (column) {
          case 0:
            return ii;
          case 1:
            return ij;
          case 2:
            return ik;
          default:
            throw new IllegalArgumentException(
                "Unable to retrieve element (" + row + ", " + column + "). Column index invalid.");

        }
      case 1:
        switch (column) {
          case 0:
            return ji;
          case 1:
            return jj;
          case 2:
            return jk;
          default:
            throw new IllegalArgumentException(
                "Unable to retrieve element (" + row + ", " + column + "). Column index invalid.");

        }

      case 2:
        switch (column) {
          case 0:
            return ki;
          case 1:
            return kj;
          case 2:
            return kk;
          default:
            throw new IllegalArgumentException(
                "Unable to retrieve element (" + row + ", " + column + "). Column index invalid.");

        }

      default:
        throw new IllegalArgumentException(
            "Unable to retrieve element (" + row + ", " + column + "). Row index invalid.");
    }

  }

  /**
   * Copies the ith column components into the supplied vector.
   * 
   * @param buffer the vector to receive the components
   * 
   * @return a reference to buffer for convenience
   */
  public final VectorIJK getIthColumn(VectorIJK buffer) {
    buffer.i = ii;
    buffer.j = ji;
    buffer.k = ki;

    return buffer;
  }

  /**
   * Extracts the ith column components as a vector.
   * 
   * @return ith column vector
   */
  public final VectorIJK getIthColumn() {
    return getIthColumn(new VectorIJK());
  }

  /**
   * Copies the jth column components into the supplied vector.
   * 
   * @param buffer the vector to receive the components
   * 
   * @return a reference to buffer for convenience
   */
  public final VectorIJK getJthColumn(VectorIJK buffer) {
    buffer.i = ij;
    buffer.j = jj;
    buffer.k = kj;

    return buffer;
  }

  /**
   * Extracts the jth column components as a vector.
   * 
   * @return jth column vector
   */
  public final VectorIJK getJthColumn() {
    return getJthColumn(new VectorIJK());
  }

  /**
   * Copies the kth column components into the supplied vector.
   * 
   * @param buffer the vector to receive the components
   * 
   * @return a reference to buffer for convenience
   */
  public final VectorIJK getKthColumn(VectorIJK buffer) {
    buffer.i = ik;
    buffer.j = jk;
    buffer.k = kk;

    return buffer;
  }

  /**
   * Extracts the kth column components as a vector.
   * 
   * @return kth column vector
   */
  public final VectorIJK getKthColumn() {
    return getKthColumn(new VectorIJK());
  }

  /**
   * Copies the desired column components into the supplied vector.
   * 
   * @param columnIndex index of the column contents to copy. Must be in [0,2]
   * @param buffer the vector to receive the components
   * 
   * @return a reference to buffer for convenience
   * 
   * @throws IllegalArgumentException if the supplied columnIndex lies outside the acceptable range
   */
  public final VectorIJK getColumn(int columnIndex, VectorIJK buffer) {

    switch (columnIndex) {
      case 0:
        return getIthColumn(buffer);

      case 1:
        return getJthColumn(buffer);

      case 2:
        return getKthColumn(buffer);

      default:
        throw new IllegalArgumentException(
            "Unable to retrieve column. Index: " + columnIndex + " is invalid.");
    }

  }

  /**
   * Extracts the desired column components as a vector.
   * 
   * @param columnIndex index of the column contents to extract. Must be in [0,2]
   * 
   * @return desired column vector
   * 
   * @throws IllegalArgumentException if the supplied columnIndex lies outside the acceptable range
   */
  public final VectorIJK getColumn(int columnIndex) {
    return getColumn(columnIndex, new VectorIJK());
  }

  /**
   * Computes the determinant of the matrix.
   * 
   * @return the determinant of the instance
   */
  public double getDeterminant() {
    return computeDeterminant(ii, ji, ki, ij, jj, kj, ik, jk, kk);
  }

  /**
   * Computes the trace of the matrix.
   * 
   * @return the trace of the instance
   */
  public double getTrace() {
    return ii + jj + kk;
  }

  /**
   * Is this matrix invertible so that the determinant does not vanish within a tolerance of
   * {@value #INVERSION_TOLERANCE}?
   * 
   * @return true if the matrix is invertible, false otherwise
   */
  public boolean isInvertible() {
    return isInvertible(INVERSION_TOLERANCE);
  }

  /**
   * Is this matrix invertible so that the determinant does not vanish within a tolerance specified
   * by <code>inversionTolerance</code>?
   * 
   * @param inversionTolerance specifies how far off zero the determinant of the instance is allowed
   *        to be
   * 
   * @return true if the matrix is invertible, false otherwise
   */
  public boolean isInvertible(double inversionTolerance) {
    double absDet = abs(getDeterminant());
    if (absDet < inversionTolerance) {
      return false;
    }
    return true;
  }

  /**
   * Are the columns of this matrix orthogonal so that the determinant (after columns have been
   * unitized) is equal <code>&pm;1</code> within a tolerance of {@value #DETERMINANT_TOLERANCE}?
   * <p>
   * If this returns true, this matrix may be more efficiently inverted using
   * {@link #createInvorted()}.
   * <p>
   * Otherwise, this matrix may only be inverted (assuming its determinant is non-zero) using
   * {@link #createInverse()}.
   * 
   * @return true if the matrix columns are orthonormal, false otherwise
   */
  public boolean hasOrthogonalColumns() {
    return hasOrthogonalColumns(DETERMINANT_TOLERANCE);
  }

  /**
   * Are the columns of this matrix orthogonal so that the determinant (after columns have been
   * unitized) is equal <code>&pm;1</code> within a tolerance of <code>determinantTolerance</code>?
   * <p>
   * If this returns true, this matrix may be more efficiently inverted using
   * {@link #createInvorted()}.
   * <p>
   * Otherwise, this matrix may only be inverted (assuming its determinant is non-zero) using
   * {@link #createInverse()}.
   * 
   * @param determinantTolerance specifies how far off unity the determinant of the instance (after
   *        columns have been unitized) is allowed to be
   * 
   * @return true if the matrix columns are orthogonal, false otherwise
   */
  public boolean hasOrthogonalColumns(double determinantTolerance) {
    return createUnitizedColumns().isOrthogonal(determinantTolerance);
  }

  /**
   * Are the columns of this matrix ortho<i>normal</i> so that the determinant is equal
   * <code>&pm;1</code> within a tolerance of {@value #DETERMINANT_TOLERANCE}?
   * 
   * @return true if the matrix columns are orthonormal, false otherwise
   */
  public boolean isOrthogonal() {
    return isOrthogonal(DETERMINANT_TOLERANCE);
  }

  /**
   * Are the columns of this matrix ortho<i>normal</i> so that the determinant is equal
   * <code>&pm;1</code> within the tolerance specified by <code>determinantTolerance</code>?
   * 
   * @param determinantTolerance specifies how far off unity the determinant of the instance is
   *        allowed to be
   * 
   * @return true if the matrix columns are orthonormal, false otherwise
   */
  public boolean isOrthogonal(double determinantTolerance) {
    double absErr = abs(abs(getDeterminant()) - 1);
    if (absErr > determinantTolerance) {
      return false;
    }
    return true;
  }

  /**
   * Do the components of the instance represent a rotation subject to the default norm
   * {@link #NORM_TOLERANCE} and determinant {@value #DETERMINANT_TOLERANCE} tolerances.
   * 
   * @return true if the matrix components capture a rotation, false otherwise
   */
  public boolean isRotation() {
    return isRotation(NORM_TOLERANCE, DETERMINANT_TOLERANCE);
  }

  /**
   * Do the components of the instance represent a rotation subject to the supplied tolerances
   * 
   * @param normTolerance specifies how far off unity the norms of the column vectors are allowed to
   *        be
   * @param determinantTolerance specifies how far off unity the determinant of the instance is
   *        allowed to be
   * 
   * @return true if the matrix components capture a rotation, false otherwise
   */
  public boolean isRotation(double normTolerance, double determinantTolerance) {
    try {
      InternalOperations.checkRotation(ii, ji, ki, ij, jj, kj, ik, jk, kk, normTolerance,
          determinantTolerance);
      return true;
    } catch (MalformedRotationException e) {
      return false;
    }
  }

  /**
   * Compute the product of this matrix with a vector.
   * 
   * @param v the vector
   * 
   * @return a new <code>VectorIJK</code> containing the result.
   * 
   * @see UnwritableMatrixIJK#mxv(UnwritableVectorIJK, VectorIJK)
   */
  public VectorIJK mxv(UnwritableVectorIJK v) {
    return mxv(v, new VectorIJK());
  }

  /**
   * Compute the product of this matrix with a vector.
   * 
   * @param v the vector
   * @param buffer the buffer to receive the product, m*v.
   * 
   * @return a reference to buffer for convenience.
   */
  public VectorIJK mxv(UnwritableVectorIJK v, VectorIJK buffer) {

    double i = ii * v.i + ij * v.j + ik * v.k;
    double j = ji * v.i + jj * v.j + jk * v.k;
    buffer.k = ki * v.i + kj * v.j + kk * v.k;
    buffer.i = i;
    buffer.j = j;
    return buffer;
  }

  /**
   * Compute the product of the transpose of a matrix with a vector.
   * 
   * @param v the vector
   * 
   * @return a new <code>VectorIJK</code> containing the result.
   * 
   * @see UnwritableMatrixIJK#mtxv(UnwritableVectorIJK, VectorIJK)
   */
  public VectorIJK mtxv(UnwritableVectorIJK v) {
    return mtxv(v, new VectorIJK());
  }

  /**
   * Compute the product of the transpose of a matrix with a vector.
   * 
   * @param v the vector
   * @param buffer the buffer to receive the product, transpose(m)*v
   * 
   * @return a reference to buffer for convenience.
   */
  public VectorIJK mtxv(UnwritableVectorIJK v, VectorIJK buffer) {

    double i = ii * v.i + ji * v.j + ki * v.k;
    double j = ij * v.i + jj * v.j + kj * v.k;
    buffer.k = ik * v.i + jk * v.j + kk * v.k;
    buffer.i = i;
    buffer.j = j;
    return buffer;
  }

  /**
   * Makes an unwritable copy of the supplied matrix.
   * <p>
   * This method makes an unwritable copy only if necessary. It tries to avoid making a copy
   * wherever possible.
   * </p>
   * 
   * @param matrix a matrix to copy.
   * 
   * @return either a reference to matrix (if matrix is already only an instance of
   *         {@link UnwritableMatrixIJK}, otherwise an unwritable copy of matrix's contents
   */
  public static UnwritableMatrixIJK copyOf(UnwritableMatrixIJK matrix) {
    if (matrix.getClass().equals(UnwritableMatrixIJK.class)) {
      return matrix;
    }
    return new UnwritableMatrixIJK(matrix);
  }

  @Override
  public final int hashCode() {
    final int prime = 31;
    int result = 1;
    long temp;
    temp = Double.doubleToLongBits(ii);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(ij);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(ik);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(ji);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(jj);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(jk);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(ki);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(kj);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(kk);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    return result;
  }

  @Override
  public final boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (!(obj instanceof UnwritableMatrixIJK)) {
      return false;
    }
    final UnwritableMatrixIJK other = (UnwritableMatrixIJK) obj;
    if (Double.doubleToLongBits(ii) != Double.doubleToLongBits(other.ii)) {
      return false;
    }
    if (Double.doubleToLongBits(ij) != Double.doubleToLongBits(other.ij)) {
      return false;
    }
    if (Double.doubleToLongBits(ik) != Double.doubleToLongBits(other.ik)) {
      return false;
    }
    if (Double.doubleToLongBits(ji) != Double.doubleToLongBits(other.ji)) {
      return false;
    }
    if (Double.doubleToLongBits(jj) != Double.doubleToLongBits(other.jj)) {
      return false;
    }
    if (Double.doubleToLongBits(jk) != Double.doubleToLongBits(other.jk)) {
      return false;
    }
    if (Double.doubleToLongBits(ki) != Double.doubleToLongBits(other.ki)) {
      return false;
    }
    if (Double.doubleToLongBits(kj) != Double.doubleToLongBits(other.kj)) {
      return false;
    }
    if (Double.doubleToLongBits(kk) != Double.doubleToLongBits(other.kk)) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return "[" + ii + "," + ji + "," + ki + ";" + ij + "," + jj + "," + kj + ";" + ik + "," + jk
        + "," + kk + "]";
  }

}
