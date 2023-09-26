package picante.math.vectorspace;

import static com.google.common.base.Preconditions.checkArgument;
import static picante.math.PicanteMath.abs;
import static picante.math.PicanteMath.max;
import static picante.math.PicanteMath.sqrt;
import static picante.math.vectorspace.InternalOperations.computeNorm;
import picante.designpatterns.Writable;

/**
 * A writable subclass of the unwritable 3D matrix parent completing one link in the
 * weak-immutability design pattern.
 * <p>
 * This class contains the mutator methods necessary to set or alter the internals of the parent
 * classes fields.
 * </p>
 * 
 * @author G.K.Stephens
 */
public class MatrixIJ extends UnwritableMatrixIJ
    implements Writable.ImplementationInterface<UnwritableMatrixIJ, MatrixIJ> {

  /**
   * The matrix whose components are all zero.
   */
  public static final UnwritableMatrixIJ ZEROS = new UnwritableMatrixIJ(0, 0, 0, 0);

  /**
   * The matrix whose components are all ones.
   */
  public static final UnwritableMatrixIJ ONES = new UnwritableMatrixIJ(1, 1, 1, 1);

  /**
   * The multiplicative identity.
   */
  public static final UnwritableMatrixIJ IDENTITY = new UnwritableMatrixIJ(1, 0, 0, 1);

  /**
   * Construct a matrix with an initial value of {@link #IDENTITY}.
   */
  public MatrixIJ() {
    super(IDENTITY);
  }

  /**
   * Constructs a matrix from the nine basic components.
   * 
   * @param ii ith row, ith column element
   * @param ji jth row, ith column element
   * @param ij ith row, jth column element
   * @param jj jth row, jth column element
   */
  public MatrixIJ(double ii, double ji, double ij, double jj) {
    super(ii, ji, ij, jj);
  }

  /**
   * Constructs a matrix from the upper three by three block of a two dimensional array of doubles.
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
   * @throws IndexOutOfBoundsException if the supplied data array does not contain at least three
   *         arrays of arrays of length three or greater.
   */
  public MatrixIJ(double[][] data) {
    super(data);
  }

  /**
   * Copy constructor, creates a matrix by copying the values of a pre-existing one.
   * 
   * @param matrix the matrix whose contents are to be copied.
   */
  public MatrixIJ(UnwritableMatrixIJ matrix) {
    super(matrix);
  }

  /**
   * Scaling constructor, creates a new matrix by applying a scalar multiple to the components of a
   * pre-existing matrix.
   * 
   * @param scale the scale factor to apply
   * @param matrix the matrix whose components are to be scaled and copied
   */
  public MatrixIJ(double scale, UnwritableMatrixIJ matrix) {
    super(scale, matrix);
  }

  /**
   * Column scaling constructor, creates a new matrix by applying scalar multiples to each column of
   * a pre-existing matrix.
   * 
   * @param scaleI the scale factor to apply to the ith column
   * @param scaleJ the scale factor to apply to the jth column
   * @param matrix the matrix whose columns are to be scaled and copied
   */
  public MatrixIJ(double scaleI, double scaleJ, UnwritableMatrixIJ matrix) {
    super(scaleI, scaleJ, matrix);
  }

  /**
   * Column vector constructor, creates a new matrix by populating the columns of the matrix with
   * the supplied vectors.
   * 
   * @param ithColumn the vector containing the ith column
   * @param jthColumn the vector containing the jth column
   */
  public MatrixIJ(UnwritableVectorIJ ithColumn, UnwritableVectorIJ jthColumn) {
    super(ithColumn, jthColumn);
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
  public MatrixIJ(double scaleI, UnwritableVectorIJ ithColumn, double scaleJ,
      UnwritableVectorIJ jthColumn) {
    super(scaleI, ithColumn, scaleJ, jthColumn);
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable subclass rather than the
   * unwritable parent.
   */
  @Override
  public MatrixIJ createTranspose() {
    return new MatrixIJ(this).transpose();
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable subclass rather than the
   * unwritable parent.
   */
  @Override
  public MatrixIJ createUnitizedColumns() {
    return new MatrixIJ(this).unitizeColumns();
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable subclass rather than the
   * unwritable parent.
   */
  @Override
  public MatrixIJ createInverse() {
    return new MatrixIJ(this).invert();
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable subclass rather than the
   * unwritable parent.
   */
  @Override
  public MatrixIJ createInverse(double tolerance) {
    return new MatrixIJ(this).invert(tolerance);
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable subclass rather than the
   * unwritable parent.
   */
  @Override
  public MatrixIJ createInvorted() {
    return new MatrixIJ(this).invort();
  }

  /**
   * Transpose the matrix.
   * 
   * @return a reference to the instance for convenience, which now contains the transpose
   */
  public MatrixIJ transpose() {
    double tmp = this.ij;
    this.ij = this.ji;
    this.ji = tmp;

    return this;
  }

  /**
   * Modifies the elements of this matrix so that each column in the matrix becomes a unit vector
   * 
   * @return a reference to the instance for convenience
   * 
   * @throws UnsupportedOperationException if any of the columns are of length zero
   */
  public MatrixIJ unitizeColumns() {
    setTo(new VectorIJ(this.ii, this.ji).unitize(), // first
        // column
        new VectorIJ(this.ij, this.jj).unitize()); // second
    // column
    return this;
  }

  /**
   * Invert the matrix if the determinant is not within the default tolerance of zero.
   * 
   * @return a reference to the instance for convenience, which now contains the multiplicative
   *         inverse
   * 
   * @throws UnsupportedOperationException if the determinant of the instance is within
   *         {@link UnwritableMatrixIJ#INVERSION_TOLERANCE} of 0.0.
   */
  public MatrixIJ invert() {
    return invert(UnwritableMatrixIJ.INVERSION_TOLERANCE);
  }

  /**
   * Invert the matrix if the determinant is within the supplied tolerance of zero.
   * 
   * @param tolerance the absolute value of the determinant of the instance must be greater than
   *        this for inversion to proceed
   * 
   * @return a reference to the instance for convenience, which now contains the multiplicative
   *         inverse
   * 
   * @throws UnsupportedOperationException if the determinant of the instance is within the supplied
   *         tolerance of 0.0.
   */
  public MatrixIJ invert(double tolerance) {

    double det = getDeterminant();

    if (abs(det) < tolerance) {
      throw new UnsupportedOperationException("Matrix nearly singular, unable to invert.");
    }

    double cii = jj / det;
    double cij = -ij / det;
    double cji = -ji / det;
    double cjj = ii / det;

    return setTo(cii, cji, cij, cjj);
  }

  /**
   * Invert, in place, this matrix whose columns are orthogonal.
   * <p>
   * Note: No checks are done to verify that the columns are orthogonal.
   * </p>
   * 
   * @return a reference to the instance for convenience
   * 
   * @throws UnsupportedOperationException if the lengths of any of the columns are zero or too
   *         small to properly invert multiplicatively in the space available to double precision
   */
  public MatrixIJ invort() {

    transpose();

    double length = computeNorm(this.ii, this.ij);

    if ((length * INVORSION_BOUND < 1) || (length == 0)) {
      throw new UnsupportedOperationException(
          "ith column of matrix has length, " + length + ", for which there is no inverse.");
    }

    this.ii /= length;
    this.ii /= length;
    this.ij /= length;
    this.ij /= length;

    length = computeNorm(this.ji, this.jj);

    if ((length * INVORSION_BOUND < 1) || (length == 0)) {
      throw new UnsupportedOperationException(
          "jth column of matrix has length, " + length + ", for which there is no inverse.");
    }

    this.ji /= length;
    this.ji /= length;
    this.jj /= length;
    this.jj /= length;

    return this;
  }

  /**
   * Scales each component of the matrix by the supplied factor.
   * 
   * @param scale the scale factor to apply
   * 
   * @return a reference to the instance which now contains the scaled matrix.
   */
  public MatrixIJ scale(double scale) {

    this.ii *= scale;
    this.ji *= scale;
    this.ij *= scale;
    this.jj *= scale;

    return this;
  }

  /**
   * Scales each column of the matrix by the supplied factors.
   * 
   * @param scaleI the ith column scale
   * @param scaleJ the jth column scale
   * 
   * @return a reference to the instance for convenience which contains the scaled matrix.
   */
  public MatrixIJ scale(double scaleI, double scaleJ) {

    this.ii *= scaleI;
    this.ji *= scaleI;
    this.ij *= scaleJ;
    this.jj *= scaleJ;

    return this;
  }

  /**
   * Sets the ith row, ith column component.
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
   */
  public final void setII(double ii) {
    this.ii = ii;
  }

  /**
   * Sets the jth row, ith column component.
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
   */
  public final void setJI(double ji) {
    this.ji = ji;
  }

  /**
   * Sets the ith row, jth column component.
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
   */
  public final void setIJ(double ij) {
    this.ij = ij;
  }

  /**
   * Sets the jth row, jth column component.
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
   */
  public final void setJJ(double jj) {
    this.jj = jj;
  }

  /**
   * Sets the component for the specified row and column.
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
   * @param row a row index in [0,1].
   * @param column a column index in [0,1]
   * @param value the value to place into the matrix at (row,column).
   * 
   * @throws IllegalArgumentException if either the supplied row or column index are outside their
   *         acceptable ranges of [0,1].
   */
  public final void set(int row, int column, double value) {
    switch (row) {
      case 0:
        switch (column) {
          case 0:
            this.ii = value;
            return;
          case 1:
            this.ij = value;
            return;
          default:
            throw new IllegalArgumentException(
                "Unable to set element (" + row + ", " + column + "). Column index invalid.");

        }
      case 1:
        switch (column) {
          case 0:
            this.ji = value;
            return;
          case 1:
            this.jj = value;
            return;
          default:
            throw new IllegalArgumentException(
                "Unable to set element (" + row + ", " + column + "). Column index invalid.");

        }
      default:
        throw new IllegalArgumentException(
            "Unable to set element (" + row + ", " + column + "). Row index invalid.");
    }

  }

  /**
   * Sets the ith column to the supplied vector.
   * <p>
   * <br>
   * <table>
   * <tr>
   * <td><b>ii</b></td>
   * <td>ij</td>
   * </tr>
   * <tr>
   * <td><b>ji</b></td>
   * <td>jj</td>
   * </tr>
   * </table>
   * <br>
   * </p>
   * 
   * @param column the vector whose components are to replace the ith column of this matrix
   */
  public final void setIthColumn(UnwritableVectorIJ column) {
    this.ii = column.i;
    this.ji = column.j;
  }

  /**
   * Sets the jth column to the supplied vector.
   * <p>
   * <br>
   * <table>
   * <tr>
   * <td>ii</td>
   * <td><b>ij</b></td>
   * </tr>
   * <tr>
   * <td>ji</td>
   * <td><b>jj</b></td>
   * </tr>
   * </table>
   * <br>
   * </p>
   * 
   * @param column the vector whose components are to replace the jth column of this matrix
   */
  public final void setJthColumn(UnwritableVectorIJ column) {
    this.ij = column.i;
    this.jj = column.j;
  }

  /**
   * Sets the column at a specified index to the supplied vector.
   * 
   * <p>
   * <br>
   * <table>
   * <tr>
   * <td>0</td>
   * <td>1</td>
   * </tr>
   * <tr>
   * <td>ii</td>
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
   * @param columnIndex a column index in [0,1].
   * @param column the vector whose components are to replace the specified column of this matrix
   * 
   * @throws IllegalArgumentException if the supplied columnIndex is not in [0,1].
   */
  public final void setColumn(int columnIndex, UnwritableVectorIJ column) {

    switch (columnIndex) {
      case 0:
        setIthColumn(column);
        return;
      case 1:
        setJthColumn(column);
        return;
      default:
        throw new IllegalArgumentException(
            "Unable to set column. Index: " + columnIndex + " is invalid.");
    }

  }

  /**
   * Sets the components of this matrix to the supplied components
   * 
   * @param ii ith row, ith column element
   * @param ji jth row, ith column element
   * @param ij ith row, jth column element
   * @param jj jth row, jth column element
   * 
   * @return a reference to the instance, for convenience, that contains the newly set matrix
   */
  public final MatrixIJ setTo(double ii, double ji, double ij, double jj) {
    this.ii = ii;
    this.ji = ji;
    this.ij = ij;
    this.jj = jj;
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
   */
  public final MatrixIJ setTo(double[][] data) {
    setTo(data[0][0], data[1][0], data[0][1], data[1][1]);
    return this;
  }

  /**
   * Sets the contents of this matrix to match those of a supplied matrix
   * 
   * @param matrix the matrix to copy
   * 
   * @return a reference to this instance for convenience that contains the supplied components
   */
  @Override
  public final MatrixIJ setTo(UnwritableMatrixIJ matrix) {
    setTo(matrix.ii, matrix.ji, matrix.ij, matrix.jj);
    return this;
  }

  /**
   * Sets the contents of this matrix to a scaled version of the supplied matrix
   * 
   * @param scale the scale factor to apply to matrix
   * 
   * @param matrix the matrix to scale
   * 
   * @return a reference to this instance for convenience that contains the scaled version of matrix
   */
  public final MatrixIJ setTo(double scale, UnwritableMatrixIJ matrix) {
    setTo(matrix);
    scale(scale);
    return this;
  }

  /**
   * Sets the contents of this matrix to a column-wise scaled version of the supplied matrix
   * 
   * @param scaleI the scale factor to apply to the ith column of matrix
   * @param scaleJ the scale factor to apply to the jth column of matrix
   * @param matrix the matrix to scale
   * 
   * @return a reference to this instance for convenience that contains the column scaled version of
   *         matrix
   */
  public final MatrixIJ setTo(double scaleI, double scaleJ, UnwritableMatrixIJ matrix) {
    setTo(matrix);
    scale(scaleI, scaleJ);
    return this;
  }

  /**
   * Sets the columns of this matrix to the three specified vectors.
   * 
   * @param ithColumn the vector containing the contents to set the ith column
   * @param jthColumn the vector containing the contents to set the jth column
   * 
   * @return a reference to the instance for convenience
   */
  public final MatrixIJ setTo(UnwritableVectorIJ ithColumn, UnwritableVectorIJ jthColumn) {
    setTo(ithColumn.i, ithColumn.j, jthColumn.i, jthColumn.j);
    return this;
  }

  /**
   * Sets the columns of this matrix to the scaled versions of the supplied vectors.
   * 
   * @param scaleI scale factor to apply to ithColumn
   * @param ithColumn the ith column vector
   * @param scaleJ scale factor to apply to jthColumn
   * @param jthColumn the jth column vector
   * 
   * @return a reference to the instance for convenience
   */
  public final MatrixIJ setTo(double scaleI, UnwritableVectorIJ ithColumn, double scaleJ,
      UnwritableVectorIJ jthColumn) {
    setTo(scaleI * ithColumn.i, scaleI * ithColumn.j, scaleJ * jthColumn.i, scaleJ * jthColumn.j);
    return this;
  }

  /**
   * Sets this matrix components to the transpose of the supplied matrix.
   * 
   * @param matrix the matrix whose transpose is to be copied into the instance
   * 
   * @return a reference to the instance for convenience
   */
  public final MatrixIJ setToTranspose(UnwritableMatrixIJ matrix) {
    setTo(matrix);
    transpose();
    return this;
  }

  /**
   * Modifies the elements of this matrix so that each column in the matrix becomes a unit vector
   * 
   * @return a reference to the instance for convenience
   * 
   * @throws UnsupportedOperationException if any of the columns are of length zero
   */
  public final MatrixIJ setToUnitizedColumns(MatrixIJ matrix) {
    return setTo(matrix).unitizeColumns();
  }

  /**
   * Sets the matrix components to the inverse of the supplied matrix.
   * 
   * @param matrix the matrix to invert
   * 
   * @return a reference to the instance containing the inverse of matrix for convenience
   * 
   * @throws IllegalArgumentException if the determinant of matrix is within
   *         {@link UnwritableMatrixIJ#INVERSION_TOLERANCE} of 0.0.
   */
  public final MatrixIJ setToInverse(UnwritableMatrixIJ matrix) {

    double det = matrix.getDeterminant();

    if (abs(det) < UnwritableMatrixIJ.DETERMINANT_TOLERANCE) {
      throw new IllegalArgumentException("Matrix nearly singular, unable to invert.");
    }

    setTo(matrix);
    invert();
    return this;
  }

  /**
   * Sets the matrix components to the inverse of the supplied matrix.
   * 
   * @param matrix the matrix to invert
   * @param tolerance the tolerance
   * 
   * @return a reference to the instance containing the inverse of matrix for convenience
   * 
   * @throws IllegalArgumentException if the determinant of matrix is within tolerance of 0.0.
   * 
   */
  public final MatrixIJ setToInverse(UnwritableMatrixIJ matrix, double tolerance) {

    double det = matrix.getDeterminant();

    if (abs(det) < tolerance) {
      throw new IllegalArgumentException("Matrix nearly singular, unable to invert.");
    }

    setTo(matrix);
    invert(tolerance);
    return this;
  }

  /**
   * Sets the instance to the inverse of the supplied matrix, assuming this matrix has columns that
   * are orthogonal.
   * 
   * @param matrix a matrix to invert, with orthogonal columns.
   * 
   * @return a reference to the instance for convenience
   * 
   * @throws UnsupportedOperationException if any of the columns are zero or too small to properly
   *         invert multiplicatively in the space available to double precision
   */
  public final MatrixIJ setToInvorted(UnwritableMatrixIJ matrix) {
    return setTo(matrix).invort();
  }

  /**
   * Compute the product of a matrix with the transpose of another matrix.
   * 
   * @param a the left hand matrix
   * @param b the right hand matrix to transpose, then multiply
   * 
   * @return a new <code>MatrixIJ</code> containing the product.
   * 
   * @see MatrixIJ#mxmt(UnwritableMatrixIJ, UnwritableMatrixIJ, MatrixIJ)
   */
  public static MatrixIJ mxmt(UnwritableMatrixIJ a, UnwritableMatrixIJ b) {
    return mxmt(a, b, new MatrixIJ());
  }

  /**
   * Compute the product of a matrix with the transpose of another matrix.
   * 
   * @param a the left hand matrix
   * @param b the right hand matrix to transpose, then multiply
   * @param buffer the buffer to receive the product, a*transpose(b).
   * 
   * @return a reference to buffer for convenience.
   */
  public static MatrixIJ mxmt(UnwritableMatrixIJ a, UnwritableMatrixIJ b, MatrixIJ buffer) {
    double ii = a.ii * b.ii + a.ij * b.ij;
    double ij = a.ii * b.ji + a.ij * b.jj;

    double ji = a.ji * b.ii + a.jj * b.ij;
    double jj = a.ji * b.ji + a.jj * b.jj;

    buffer.ii = ii;
    buffer.ij = ij;

    buffer.ji = ji;
    buffer.jj = jj;

    return buffer;
  }

  /**
   * Compute the product of a transpose of a matrix with another matrix.
   * 
   * @param a the left hand matrix to transpose, then multiply
   * @param b the right hand matrix
   * 
   * @return a new <code>MatrixIJ</code> containing the product
   * 
   * @see MatrixIJ#mtxm(UnwritableMatrixIJ, UnwritableMatrixIJ, MatrixIJ)
   */
  public static MatrixIJ mtxm(UnwritableMatrixIJ a, UnwritableMatrixIJ b) {
    return mtxm(a, b, new MatrixIJ());
  }

  /**
   * Compute the product of a transpose of a matrix with another matrix.
   * 
   * @param a the left hand matrix to transpose, then multiply
   * @param b the right hand matrix
   * @param buffer the buffer to receive the product, transpose(a)*b.
   * 
   * @return a reference to buffer for convenience
   */
  public static MatrixIJ mtxm(UnwritableMatrixIJ a, UnwritableMatrixIJ b, MatrixIJ buffer) {
    double ii = a.ii * b.ii + a.ji * b.ji;
    double ij = a.ii * b.ij + a.ji * b.jj;

    double ji = a.ij * b.ii + a.jj * b.ji;
    double jj = a.ij * b.ij + a.jj * b.jj;

    buffer.ii = ii;
    buffer.ij = ij;

    buffer.ji = ji;
    buffer.jj = jj;

    return buffer;
  }

  /**
   * Compute the product of two matrices.
   * 
   * @param a the left hand matrix
   * @param b the right hand matrix
   * 
   * @return a new <code>MatrixIJ</code> containing the product (ab).
   * 
   * @see MatrixIJ#mxm(UnwritableMatrixIJ, UnwritableMatrixIJ, MatrixIJ)
   */
  public static MatrixIJ mxm(UnwritableMatrixIJ a, UnwritableMatrixIJ b) {
    return mxm(a, b, new MatrixIJ());
  }

  /**
   * Compute the product of two matrices.
   * 
   * @param a the left hand matrix
   * @param b the right hand matrix
   * @param buffer the buffer to receive the product, a*b
   * 
   * @return a reference to buffer for convenience
   */
  public static MatrixIJ mxm(UnwritableMatrixIJ a, UnwritableMatrixIJ b, MatrixIJ buffer) {
    double ii = a.ii * b.ii + a.ij * b.ji;
    double ij = a.ii * b.ij + a.ij * b.jj;

    double ji = a.ji * b.ii + a.jj * b.ji;
    double jj = a.ji * b.ij + a.jj * b.jj;

    buffer.ii = ii;
    buffer.ij = ij;

    buffer.ji = ji;
    buffer.jj = jj;

    return buffer;
  }

  /**
   * Compute the sum of a pair of matrices multipled with another matrix transposed
   * 
   * @param a left hand matrix in the first product
   * @param b right hand matrix to transpose in the first product
   * @param c left hand matrix in the second product
   * @param d right hand matrix to transpose in the second product
   * 
   * @return a new <code>MatrixIJ</code> containing (a x bt) + (c x dt)
   * 
   * @see MatrixIJ#mxmtadd(UnwritableMatrixIJ, UnwritableMatrixIJ, UnwritableMatrixIJ,
   *      UnwritableMatrixIJ, MatrixIJ)
   */
  public static MatrixIJ mxmtadd(UnwritableMatrixIJ a, UnwritableMatrixIJ b, UnwritableMatrixIJ c,
      UnwritableMatrixIJ d) {
    return mxmtadd(a, b, c, d, new MatrixIJ());
  }

  /**
   * Compute the sum of a pair of matrices multipled with another matrix transposed
   * 
   * @param a left hand matrix in the first product
   * @param b right hand matrix to transpose in the first product
   * @param c left hand matrix in the second product
   * @param d right hand matrix to transpose in the second product
   * @param buffer buffer to receive the results of (a x bt) + (c x dt)
   * 
   * @return reference to buffer for convenience
   */
  public static MatrixIJ mxmtadd(UnwritableMatrixIJ a, UnwritableMatrixIJ b, UnwritableMatrixIJ c,
      UnwritableMatrixIJ d, MatrixIJ buffer) {

    double ii = a.ii * b.ii + a.ij * b.ij;
    double ij = a.ii * b.ji + a.ij * b.jj;

    double ji = a.ji * b.ii + a.jj * b.ij;
    double jj = a.ji * b.ji + a.jj * b.jj;

    ii += c.ii * d.ii + c.ij * d.ij;
    ij += c.ii * d.ji + c.ij * d.jj;

    ji += c.ji * d.ii + c.jj * d.ij;
    jj += c.ji * d.ji + c.jj * d.jj;

    buffer.ii = ii;
    buffer.ij = ij;

    buffer.ji = ji;
    buffer.jj = jj;

    return buffer;

  }

  /**
   * Compute the sum of a pair of matrix transposes multipled with another matrix
   * 
   * @param a left hand matrix to transpose in the first product
   * @param b right hand matrix in the first product
   * @param c left hand matrix to transpose in the second product
   * @param d right hand matrix in the second product
   * 
   * @return a new <code>MatrixIJ</code> containing (at x b) + (ct x d)
   * 
   * @see MatrixIJ#mtxmadd(UnwritableMatrixIJ, UnwritableMatrixIJ, UnwritableMatrixIJ,
   *      UnwritableMatrixIJ, MatrixIJ)
   */
  public static MatrixIJ mtxmadd(UnwritableMatrixIJ a, UnwritableMatrixIJ b, UnwritableMatrixIJ c,
      UnwritableMatrixIJ d) {
    return mtxmadd(a, b, c, d, new MatrixIJ());
  }

  /**
   * Compute the sum of a pair of matrix transposes multipled with another matrix
   * 
   * @param a left hand matrix to transpose in the first product
   * @param b right hand matrix in the first product
   * @param c left hand matrix to transpose in the second product
   * @param d right hand matrix in the second product
   * @param buffer buffer to receive the results of (at x b) + (ct x d)
   * 
   * @return reference to buffer for convenience
   */
  public static MatrixIJ mtxmadd(UnwritableMatrixIJ a, UnwritableMatrixIJ b, UnwritableMatrixIJ c,
      UnwritableMatrixIJ d, MatrixIJ buffer) {

    double ii = a.ii * b.ii + a.ji * b.ji;
    double ij = a.ii * b.ij + a.ji * b.jj;

    double ji = a.ij * b.ii + a.jj * b.ji;
    double jj = a.ij * b.ij + a.jj * b.jj;

    ii += c.ii * d.ii + c.ji * d.ji;
    ij += c.ii * d.ij + c.ji * d.jj;

    ji += c.ij * d.ii + c.jj * d.ji;
    jj += c.ij * d.ij + c.jj * d.jj;

    buffer.ii = ii;
    buffer.ij = ij;

    buffer.ji = ji;
    buffer.jj = jj;

    return buffer;

  }

  /**
   * Compute the sum of the products of two pairs of matrices.
   * 
   * @param a left hand matrix in first product
   * @param b right hand matrix in first product
   * @param c left hand matrix in second product
   * @param d right hand matrix in second product
   * 
   * @return a new <code>MatrixIJ</code> containing (a x b) + (c x d)
   * 
   * @see MatrixIJ#mxmadd(UnwritableMatrixIJ, UnwritableMatrixIJ, UnwritableMatrixIJ,
   *      UnwritableMatrixIJ, MatrixIJ)
   */
  public static MatrixIJ mxmadd(UnwritableMatrixIJ a, UnwritableMatrixIJ b, UnwritableMatrixIJ c,
      UnwritableMatrixIJ d) {
    return mxmadd(a, b, c, d, new MatrixIJ());
  }

  /**
   * Compute the sum of the products of two pairs of matrices.
   * 
   * @param a left hand matrix in first product
   * @param b right hand matrix in first product
   * @param c left hand matrix in second product
   * @param d right hand matrix in second product
   * @param buffer buffer to receive the results of (a x b) + (c x d)
   * 
   * @return a reference to buffer for convenience
   */
  public static MatrixIJ mxmadd(UnwritableMatrixIJ a, UnwritableMatrixIJ b, UnwritableMatrixIJ c,
      UnwritableMatrixIJ d, MatrixIJ buffer) {

    double ii = a.ii * b.ii + a.ij * b.ji;
    double ij = a.ii * b.ij + a.ij * b.jj;

    double ji = a.ji * b.ii + a.jj * b.ji;
    double jj = a.ji * b.ij + a.jj * b.jj;

    ii += c.ii * d.ii + c.ij * d.ji;
    ij += c.ii * d.ij + c.ij * d.jj;

    ji += c.ji * d.ii + c.jj * d.ji;
    jj += c.ji * d.ij + c.jj * d.jj;

    buffer.ii = ii;
    buffer.ij = ij;

    buffer.ji = ji;
    buffer.jj = jj;

    return buffer;

  }

  /**
   * Compute the component-wise difference of two matrices.
   * 
   * @param a the minuend matrix
   * @param b the subtrahend matrix
   * 
   * @return a new <code>MatrixIJ</code> which contains (a - b)
   * 
   * @see MatrixIJ#subtract(UnwritableMatrixIJ, UnwritableMatrixIJ, MatrixIJ)
   */
  public static MatrixIJ subtract(UnwritableMatrixIJ a, UnwritableMatrixIJ b) {
    return subtract(a, b, new MatrixIJ());
  }

  /**
   * Compute the component-wise difference of two matrices.
   * 
   * @param a the minuend matrix
   * @param b the subtrahend matrix
   * @param buffer the buffer to receive the results of the subtraction
   * 
   * @return a reference to buffer for convenience which now contains (a - b)
   */
  public static MatrixIJ subtract(UnwritableMatrixIJ a, UnwritableMatrixIJ b, MatrixIJ buffer) {
    buffer.ii = a.ii - b.ii;
    buffer.ji = a.ji - b.ji;
    buffer.ij = a.ij - b.ij;
    buffer.jj = a.jj - b.jj;

    return buffer;
  }

  /**
   * Compute component-wise sum of two matrices.
   * 
   * @param a a matrix
   * @param b another matrix
   * 
   * @return a new <code>MatrixIJ</code> containing (a + b)
   * 
   * @see MatrixIJ#add(UnwritableMatrixIJ, UnwritableMatrixIJ, MatrixIJ)
   */
  public static MatrixIJ add(UnwritableMatrixIJ a, UnwritableMatrixIJ b) {
    return add(a, b, new MatrixIJ());
  }

  /**
   * Compute component-wise sum of two matrices.
   * 
   * @param a a matrix
   * @param b another matrix
   * @param buffer the buffer to receive a + b
   * 
   * @return a reference to buffer for convenience
   */
  public static MatrixIJ add(UnwritableMatrixIJ a, UnwritableMatrixIJ b, MatrixIJ buffer) {
    buffer.ii = a.ii + b.ii;
    buffer.ji = a.ji + b.ji;
    buffer.ij = a.ij + b.ij;
    buffer.jj = a.jj + b.jj;

    return buffer;
  }

  /**
   * Diagonalizes a symmetric matrix.
   * <p>
   * Diagonalization converts a matrix from it's symmetric form to an equivalent representation:
   * 
   * <pre>
   *                         T
   *    diagonalized = rotate  * symmetricMatrix * rotate
   * </pre>
   * 
   * where diagonlized is a matrix of the form:
   * 
   * <pre>
   *                    [ a   0 ]
   *     diagonalized = [       ]
   *                    [ 0   b ]
   * </pre>
   * 
   * and (a,b) are the eigenvalues of the matrix, and the columns of rotate are the eigenvectors
   * corresponding to (a,b) respectively.
   * 
   * </p>
   * 
   * @param symmetricMatrix a symmetric matrix
   * @param eigenvalueBuffer the buffer to capture the eigenvalues
   * @param eigenvectorBuffer the buffer to capture the eigenvectors, may overwrite symmetricMatrix
   * 
   * @return a reference to eigenvectorBuffer for convenience
   * 
   * @throws IllegalArgumentException if symmetricMatrix is not symmetric, i.e.
   *         symmetricMatrix.isSymmetric() is false.
   */
  public static MatrixIJ diagonalizeSymmetricMatrix(UnwritableMatrixIJ symmetricMatrix,
      VectorIJ eigenvalueBuffer, MatrixIJ eigenvectorBuffer) {

    checkArgument(symmetricMatrix.isSymmetric(), "Only able to diagonlize symmetric matrices");

    /*
     * Is the matrix already diagonal? If so, then don't do any heavy lifting.
     */
    if (symmetricMatrix.ij == 0.0) {
      eigenvalueBuffer.setTo(symmetricMatrix.ii, symmetricMatrix.jj);
      eigenvectorBuffer.setTo(MatrixIJ.IDENTITY);
      return eigenvectorBuffer;
    }

    /*
     * We only are going to use the upper triangle of the matrix. Determine a scale factor to
     * improve numerical robustness.
     */
    double scale =
        max(abs(symmetricMatrix.ii), max(abs(symmetricMatrix.ij), abs(symmetricMatrix.jj)));

    double a = symmetricMatrix.ii / scale;
    double b = symmetricMatrix.ij / scale;
    double c = symmetricMatrix.jj / scale;

    /*
     * Compute the eigenvalues of the scaled version of symmetricMatrix. The eigenvalues are simply
     * the roots of the equation:
     * 
     * determinant ( (1/scale) * symmetricMatrix - x * MatrixIJ.IDENTITY ) = 0
     * 
     * or equivalently:
     * 
     * x*x - (a+c) *x + (ac - b*b) = 0
     */
    solveQuadratic(1.0, -(a + c), a * c - b * b, eigenvalueBuffer);

    double eigval1 = eigenvalueBuffer.i;
    double eigval2 = eigenvalueBuffer.j;

    /*
     * The ith component of the eigenvalueBuffer is the root corresponding to the positive
     * discriminant term; this is guaranteed by method used to solve the quadratic equation. Now
     * find the eigenvector corresponding to the eigenvalue of the smaller magnitude. We can unitize
     * it and select an orthogonal unit vector so as to create the desired rotation matrix.
     * 
     * There are two candidate eigenvectors, select the one involving the eigenvector of the larger
     * magnitude.
     */
    if ((abs(eigval1 - a)) >= (abs(eigval1) - c)) {

      /*
       * In this case the second eigenvector component should be larger than |b|. Use Math.max()
       * below to guard against reversal of the inequality due to round-off error. Abuse the
       * eigenvalueBuffer temporarily to hold the eigenvector.
       */
      eigenvalueBuffer.setTo(b, max(eigval1 - a, abs(b)));
      eigenvalueBuffer.unitize();

      eigenvectorBuffer.setTo(eigenvalueBuffer.getJ(), -eigenvalueBuffer.getI(),
          eigenvalueBuffer.getI(), eigenvalueBuffer.getJ());

      /*
       * Swap the eigenvalues.
       */
      eigenvalueBuffer.setTo(eigval2, eigval1);

    } else {

      eigenvalueBuffer.setTo(max(eigval1 - c, abs(b)), b);
      eigenvalueBuffer.unitize();

      eigenvectorBuffer.setTo(eigenvalueBuffer.getI(), eigenvalueBuffer.getJ(),
          -eigenvalueBuffer.getJ(), eigenvalueBuffer.getI());

      /*
       * Restore the eigenvalues into their buffer.
       */
      eigenvalueBuffer.setTo(eigval1, eigval2);
    }

    /*
     * Scale the eigenvalues back up to their appropriate values.
     */
    eigenvalueBuffer.scale(scale);

    return eigenvectorBuffer;

  }

  /**
   * Solves a quadratic equation of the form:
   * 
   * <pre>
   *         2
   *    a * x  + b * x + c = 0
   * </pre>
   * 
   * @param a the quadratic coefficient
   * @param b the linear coefficient
   * @param c the constant coefficient
   * 
   * @param buffer the buffer to receive the real roots, the ith component will contain the positive
   *        discriminant term and the jth the negative.
   * 
   * @return a reference to buffer for convenience
   * 
   * @throws IllegalArgumentException if a and b are both 0.0, or if the roots are complex.
   */
  static VectorIJ solveQuadratic(double a, double b, double c, VectorIJ buffer) {

    checkArgument((a != 0.0) || (b != 0.0),
        "Both the linear and quadratic degree coefficients are zero.");

    double scale = max(abs(a), max(abs(b), abs(c)));

    /*
     * If the coefficients can be scaled without zeroing any of them out, do so. The expression
     * below only evaluates to true when the input variables can be safely scaled.
     */
    if (!(((a != 0.0) && (a / scale == 0.0)) || ((b != 0.0) && (b / scale == 0.0))
        || ((c != 0.0) && (c / scale == 0.0)))) {
      a /= scale;
      b /= scale;
      c /= scale;
    }

    /*
     * If the second degree coefficient is non-zero then we have a quadratic equation that needs
     * factoring.
     */
    if (a != 0.0) {

      double discriminant = b * b - 4 * a * c;

      /*
       * Verify that the discriminant is positive or zero.
       */
      checkArgument(discriminant >= 0.0, "Roots are not real.");

      /*
       * Take advantage of the fact that c/a is the product of the roots to improve the accuracy of
       * the root having the smaller magnitude. Compute the larger root first and then divide by c/a
       * by it to obtain the smaller root.
       */
      if (b < 0.0) {

        /*
         * The ith component will contain the root of the larger magnitude.
         */
        buffer.setI((-b + sqrt(discriminant)) / (2.0 * a));
        buffer.setJ((c / a) / buffer.getI());
      } else if (b > 0.0) {

        /*
         * The jth component will contain the root of the larger magnitude.
         */
        buffer.setJ((-b - sqrt(discriminant)) / (2.0 * a));
        buffer.setI((c / a) / buffer.getJ());
      } else {

        /*
         * The roots have the same magnitude.
         */
        buffer.setI(sqrt(discriminant) / (2.0 * a));
        buffer.setJ(-buffer.getI());
      }

      return buffer;

    }

    /*
     * If we reach here, then the quadratic coefficient is zero, implying this is a simple linear
     * equation. Since there is only one solution, set them both to the same value, the root.
     */
    buffer.setI(-c / b);
    buffer.setJ(buffer.getI());
    return buffer;
  }

  /**
   * Compute the product of the transpose of a matrix with a vector.
   * 
   * @param m the matrix
   * @param v the vector
   * 
   * @return a new <code>VectorIJ</code> containing the result.
   * 
   * @see UnwritableMatrixIJ#mtxv(UnwritableVectorIJ)
   */
  @Deprecated
  public static VectorIJ mtxv(UnwritableMatrixIJ m, UnwritableVectorIJ v) {
    return m.mtxv(v, new VectorIJ());
  }

  /**
   * Compute the product of the transpose of a matrix with a vector.
   * 
   * @param m the matrix
   * @param v the vector
   * @param buffer the buffer to receive the product, transpose(m)*v
   * 
   * @return a reference to buffer for convenience.
   * 
   * @see UnwritableMatrixIJ#mtxv(UnwritableVectorIJ, VectorIJ)
   */
  @Deprecated
  public static VectorIJ mtxv(UnwritableMatrixIJ m, UnwritableVectorIJ v, VectorIJ buffer) {
    return m.mtxv(v, buffer);
  }

  /**
   * Compute the product of a matrix with a vector.
   * 
   * @param m the matrix
   * @param v the vector
   * 
   * @return a new <code>VectorIJ</code> containing the result.
   * 
   * @see UnwritableMatrixIJ#mxv(UnwritableVectorIJ)
   */
  @Deprecated
  public static VectorIJ mxv(UnwritableMatrixIJ m, UnwritableVectorIJ v) {
    return m.mxv(v, new VectorIJ());
  }

  /**
   * Compute the product of a matrix with a vector.
   * 
   * @param m the matrix
   * @param v the vector
   * @param buffer the buffer to receive the product, m*v.
   * 
   * @return a reference to buffer for convenience.
   * 
   * @see UnwritableMatrixIJ#mxv(UnwritableVectorIJ, VectorIJ)
   */
  @Deprecated
  public static VectorIJ mxv(UnwritableMatrixIJ m, UnwritableVectorIJ v, VectorIJ buffer) {
    return m.mxv(v, buffer);
  }

}
