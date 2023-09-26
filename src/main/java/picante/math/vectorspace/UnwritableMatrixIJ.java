package picante.math.vectorspace;

import static picante.math.PicanteMath.abs;
import static picante.math.vectorspace.InternalOperations.computeDeterminant;
import static picante.math.vectorspace.InternalOperations.computeNorm;

/**
 * A weakly immutable 2-dimensional matrix designed to properly support several writable subclasses.
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
 * </tr>
 * <tr>
 * <td>ji</td>
 * <td>jj</td>>
 * </tr>
 * </table>
 * <br>
 * The class prefers column ordering, so when working with vectors and this class they are to be
 * considered &quot;column&quot; vectors.
 * </p>
 * 
 * @author G.K.Stephens
 */
public class UnwritableMatrixIJ {

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
   * The jth row, ith column component of the matrix.
   */
  protected double ji;

  /**
   * The jth row, jth column component of the matrix.
   */
  protected double jj;

  /**
   * Protected, no argument, no initialization constructor for subclasses to utilize.
   */
  protected UnwritableMatrixIJ() {
    super();
  }

  /**
   * Constructs a matrix from the four basic components.
   * 
   * @param ii ith row, ith column element
   * @param ji jth row, ith column element
   * @param ij ith row, jth column element
   * @param jj jth row, jth column element
   */
  public UnwritableMatrixIJ(double ii, double ji, double ij, double jj) {
    super();
    this.ii = ii;
    this.ji = ji;
    this.ij = ij;
    this.jj = jj;
  }

  /**
   * Constructs a matrix from the upper two by two block of a two dimensional array of doubles.
   * <p>
   * The values from the data array are copied into the matrix as follows: <br>
   * <table>
   * <tr>
   * <td>data[0][0]</td>
   * <td>data[0][1]</td>
   * </tr>
   * <tr>
   * <td>data[1][0]</td>
   * <td>data[1][1]</td>
   * </tr>
   * </table>
   * </p>
   * 
   * @param data the array of doubles
   * 
   * @throws IndexOutOfBoundsException if the supplied data array does not contain at least two
   *         arrays of arrays of length two or greater.
   */
  public UnwritableMatrixIJ(double[][] data) {
    this(data[0][0], data[1][0], data[0][1], data[1][1]);
  }

  /**
   * Copy constructor, creates a matrix by copying the values of a pre-existing one.
   * 
   * @param matrix the matrix whose contents are to be copied.
   */
  public UnwritableMatrixIJ(UnwritableMatrixIJ matrix) {
    this(matrix.ii, matrix.ji, matrix.ij, matrix.jj);
  }

  /**
   * Scaling constructor, creates a new matrix by applying a scalar multiple to the components of a
   * pre-existing matrix.
   * 
   * @param scale the scale factor to apply
   * @param matrix the matrix whose components are to be scaled and copied
   */
  public UnwritableMatrixIJ(double scale, UnwritableMatrixIJ matrix) {
    this(scale * matrix.ii, scale * matrix.ji, scale * matrix.ij, scale * matrix.jj);
  }

  /**
   * Column scaling constructor, creates a new matrix by applying scalar multiples to the columns of
   * a pre-existing matrix.
   * 
   * @param scaleI scale factor to apply to the ith column
   * @param scaleJ scale factor to apply to the jth column
   * @param matrix the matrix whose components are to be scaled and copied
   */
  public UnwritableMatrixIJ(double scaleI, double scaleJ, UnwritableMatrixIJ matrix) {
    this(scaleI * matrix.ii, scaleI * matrix.ji, scaleJ * matrix.ij, scaleJ * matrix.jj);
  }

  /**
   * Column vector constructor, creates a new matrix by populating the columns of the matrix with
   * the supplied vectors.
   * 
   * @param ithColumn the vector containing the ith column
   * @param jthColumn the vector containing the jth column
   */
  public UnwritableMatrixIJ(UnwritableVectorIJ ithColumn, UnwritableVectorIJ jthColumn) {
    this(ithColumn.i, ithColumn.j, jthColumn.i, jthColumn.j);
  }

  /**
   * Scaled column vector constructor, creates a new matrix by populating the columns of the matrix
   * with scaled versions of the supplied vectors
   * 
   * @param scaleI the scale factor to apply to the ith column
   * @param ithColumn the vector containing the ith column
   * @param scaleJ the scale factor to apply to the jth column
   * @param jthColumn the vector containing the jth column
   */
  public UnwritableMatrixIJ(double scaleI, UnwritableVectorIJ ithColumn, double scaleJ,
      UnwritableVectorIJ jthColumn) {
    this(scaleI * ithColumn.i, scaleI * ithColumn.j, scaleJ * jthColumn.i, scaleJ * jthColumn.j);

  }

  /**
   * Creates a new, transposed copy of the existing matrix.
   * 
   * @return the transpose of the instance
   */
  public UnwritableMatrixIJ createTranspose() {
    return new UnwritableMatrixIJ(this.ii, this.ij, this.ji, this.jj);
  }

  /**
   * Creates a new matrix whose columns are unitized versions of the columns of this matrix.
   * 
   * @return the unitized column version of this matrix
   * 
   * @throws UnsupportedOperationException if any of the columns are of length zero
   */
  public UnwritableMatrixIJ createUnitizedColumns() {
    return new UnwritableMatrixIJ(new VectorIJ(this.ii, this.ji).unitize(), // first
        // column
        new VectorIJ(this.ij, this.jj).unitize()); // second // column
  }

  /**
   * Creates a new, inverted copy of the existing matrix if possible.
   * 
   * @return the multiplicative inverse of the instance
   * 
   * @throws UnsupportedOperationException if the instance matrix has a determinant within
   *         {@value #INVERSION_TOLERANCE} of 0.0
   */
  public UnwritableMatrixIJ createInverse() {
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
  public UnwritableMatrixIJ createInverse(double tolerance) {
    double det = getDeterminant();

    if (abs(det) < tolerance) {
      throw new UnsupportedOperationException("Matrix nearly singular, unable to invert.");
    }

    return new UnwritableMatrixIJ(jj / det, -ji / det, -ij / det, ii / det);
  }

  /**
   * Creates a new, inverted copy of the existing matrix with orthogonal columns.
   * <p>
   * If this method is invoked on matrices whose columns are not orthogonal, the resultant matrix is
   * likely not the inverse sought. Use the more general {@link UnwritableMatrixIJ#createInverse()}
   * method instead.
   * </p>
   * 
   * @return a newly created matrix, that is the inverse of this matrix if it meets the
   *         orthogonality condition
   * 
   * @throws UnsupportedOperationException if the lengths of any of the columns are zero or too
   *         small to properly invert multiplicatively in the space available to double precision.
   */
  public UnwritableMatrixIJ createInvorted() {

    /*
     * First create the transpose, then all that's left is to scale the rows appropriately.
     */
    UnwritableMatrixIJ matrix = this.createTranspose();

    double length = computeNorm(matrix.ii, matrix.ij);

    if ((length * INVORSION_BOUND < 1) || (length == 0)) {
      throw new UnsupportedOperationException(
          "ith column of matrix has length, " + length + ", for which there is no inverse.");
    }

    matrix.ii /= length;
    matrix.ii /= length;
    matrix.ij /= length;
    matrix.ij /= length;

    length = computeNorm(matrix.ji, matrix.jj);

    if ((length * INVORSION_BOUND < 1) || (length == 0)) {
      throw new UnsupportedOperationException(
          "jth column of matrix has length, " + length + ", for which there is no inverse.");
    }

    matrix.ji /= length;
    matrix.ji /= length;
    matrix.jj /= length;
    matrix.jj /= length;

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
   * </tr>
   * <tr>
   * <td>ji</td>
   * <td>jj</td>
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
   * </tr>
   * <tr>
   * <td><b>ji<b></td>
   * <td>jj</td>
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
   * Gets the ith row, jth column component.
   * <p>
   * <br>
   * <table>
   * <tr>
   * <td>ii</td>
   * <td><b>ij</b></td>
   * </tr>
   * <tr>
   * <td>ji</td>
   * <td>jj</td>
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
   * </tr>
   * <tr>
   * <td>ji</td>
   * <td><b>jj</b></td>
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
   * Gets the component from the specified row and column.
   * 
   * <p>
   * <br>
   * <table>
   * <tr>
   * <td>ii (0,0)</td>
   * <td>ij (0,1)</td>
   * </tr>
   * <tr>
   * <td>ji (1,0)</td>
   * <td>jj (1,1)</td>
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
  public final VectorIJ getIthColumn(VectorIJ buffer) {
    buffer.i = ii;
    buffer.j = ji;

    return buffer;
  }

  /**
   * Copies the jth column components into the supplied vector.
   * 
   * @param buffer the vector to receive the components
   * 
   * @return a reference to buffer for convenience
   */
  public final VectorIJ getJthColumn(VectorIJ buffer) {
    buffer.i = ij;
    buffer.j = jj;

    return buffer;
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
  public final VectorIJ getColumn(int columnIndex, VectorIJ buffer) {

    switch (columnIndex) {
      case 0:
        return getIthColumn(buffer);

      case 1:
        return getJthColumn(buffer);

      default:
        throw new IllegalArgumentException(
            "Unable to retrieve column. Index: " + columnIndex + " is invalid.");
    }

  }

  /**
   * Computes the determinant of the matrix.
   * 
   * @return the determinant of the instance
   */
  public double getDeterminant() {
    return computeDeterminant(ii, ji, ij, jj);
  }

  /**
   * Computes the trace of the matrix.
   * 
   * @return the trace of the instance
   */
  public double getTrace() {
    return ii + jj;
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
      InternalOperations.checkRotation(ii, ji, ij, jj, normTolerance, determinantTolerance);
      return true;
    } catch (MalformedRotationException e) {
      return false;
    }
  }

  /**
   * Are the components of the instance symmetric.
   * 
   * @return true if this.ji == this.ij, false otherwise.
   */
  public boolean isSymmetric() {
    return ji == ij;
  }

  /**
   * Compute the product of this matrix with a vector.
   * 
   * @param v the vector
   * 
   * @return a new <code>VectorIJ</code> containing the result.
   * 
   * @see mxv
   */
  public VectorIJ mxv(UnwritableVectorIJ v) {
    return mxv(v, new VectorIJ());
  }

  /**
   * Compute the product of this matrix with a vector.
   * 
   * @param v the vector
   * @param buffer the buffer to receive the product, m*v.
   * 
   * @return a reference to buffer for convenience.
   */
  public VectorIJ mxv(UnwritableVectorIJ v, VectorIJ buffer) {

    double i = ii * v.i + ij * v.j;
    double j = ji * v.i + jj * v.j;
    buffer.i = i;
    buffer.j = j;
    return buffer;
  }

  /**
   * Compute the product of the transpose of a matrix with a vector.
   * 
   * @param v the vector
   * 
   * @return a new <code>VectorIJ</code> containing the result.
   * 
   * @see mtxv
   */
  public VectorIJ mtxv(UnwritableVectorIJ v) {
    return mtxv(v, new VectorIJ());
  }

  /**
   * Compute the product of the transpose of a matrix with a vector.
   * 
   * @param v the vector
   * @param buffer the buffer to receive the product, transpose(m)*v
   * 
   * @return a reference to buffer for convenience.
   */
  public VectorIJ mtxv(UnwritableVectorIJ v, VectorIJ buffer) {

    double i = ii * v.i + ji * v.j;
    double j = ij * v.i + jj * v.j;
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
   *         {@link UnwritableMatrixIJ}, otherwise an unwritable copy of matrix's contents
   */
  public static UnwritableMatrixIJ copyOf(UnwritableMatrixIJ matrix) {
    if (matrix.getClass().equals(UnwritableMatrixIJ.class)) {
      return matrix;
    }
    return new UnwritableMatrixIJ(matrix);
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
    temp = Double.doubleToLongBits(ji);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(jj);
    result = prime * result + (int) (temp ^ (temp >>> 32));
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
    if (!(obj instanceof UnwritableMatrixIJ)) {
      return false;
    }
    final UnwritableMatrixIJ other = (UnwritableMatrixIJ) obj;
    if (Double.doubleToLongBits(ii) != Double.doubleToLongBits(other.ii)) {
      return false;
    }
    if (Double.doubleToLongBits(ij) != Double.doubleToLongBits(other.ij)) {
      return false;
    }
    if (Double.doubleToLongBits(ji) != Double.doubleToLongBits(other.ji)) {
      return false;
    }
    if (Double.doubleToLongBits(jj) != Double.doubleToLongBits(other.jj)) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return "[" + ii + "," + ji + ";" + ij + "," + jj + "]";
  }

}
