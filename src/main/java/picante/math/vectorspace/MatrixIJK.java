package picante.math.vectorspace;

import static picante.math.PicanteMath.abs;
import static picante.math.vectorspace.InternalOperations.computeNorm;
import picante.designpatterns.Writable;

/**
 * A writable subclass of the unwritable 3D matrix parent completing one link in the
 * weak-immutability design pattern.
 * <p>
 * This class contains the mutator methods necessary to set or alter the internals of the parent
 * classes fields.
 * </p>
 */
public class MatrixIJK extends UnwritableMatrixIJK
    implements Writable.ImplementationInterface<UnwritableMatrixIJK, MatrixIJK> {

  /**
   * The matrix whose components are all zero.
   */
  public static final UnwritableMatrixIJK ZEROS =
      new UnwritableMatrixIJK(0, 0, 0, 0, 0, 0, 0, 0, 0);

  /**
   * The matrix whose components are all ones.
   */
  public static final UnwritableMatrixIJK ONES = new UnwritableMatrixIJK(1, 1, 1, 1, 1, 1, 1, 1, 1);

  /**
   * The multiplicative identity.
   */
  public static final UnwritableMatrixIJK IDENTITY =
      new UnwritableMatrixIJK(1, 0, 0, 0, 1, 0, 0, 0, 1);

  /**
   * Construct a matrix with an initial value of {@link #IDENTITY}.
   */
  public MatrixIJK() {
    super(IDENTITY);
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
  public MatrixIJK(double ii, double ji, double ki, double ij, double jj, double kj, double ik,
      double jk, double kk) {
    super(ii, ji, ki, ij, jj, kj, ik, jk, kk);
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
  public MatrixIJK(double[][] data) {
    super(data);
  }

  /**
   * Copy constructor, creates a matrix by copying the values of a pre-existing one.
   * 
   * @param matrix the matrix whose contents are to be copied.
   */
  public MatrixIJK(UnwritableMatrixIJK matrix) {
    super(matrix);
  }

  /**
   * Scaling constructor, creates a new matrix by applying a scalar multiple to the components of a
   * pre-existing matrix.
   * 
   * @param scale the scale factor to apply
   * @param matrix the matrix whose components are to be scaled and copied
   */
  public MatrixIJK(double scale, UnwritableMatrixIJK matrix) {
    super(scale, matrix);
  }

  /**
   * Column scaling constructor, creates a new matrix by applying scalar multiples to each column of
   * a pre-existing matrix.
   * 
   * @param scaleI the scale factor to apply to the ith column
   * @param scaleJ the scale factor to apply to the jth column
   * @param scaleK the scale factor to apply to the kth column
   * @param matrix the matrix whose columns are to be scaled and copied
   */
  public MatrixIJK(double scaleI, double scaleJ, double scaleK, UnwritableMatrixIJK matrix) {
    super(scaleI, scaleJ, scaleK, matrix);
  }

  /**
   * Column vector constructor, creates a new matrix by populating the columns of the matrix with
   * the supplied vectors.
   * 
   * @param ithColumn the vector containing the ith column
   * @param jthColumn the vector containing the jth column
   * @param kthColumn the vector containing the kth column
   */
  public MatrixIJK(UnwritableVectorIJK ithColumn, UnwritableVectorIJK jthColumn,
      UnwritableVectorIJK kthColumn) {
    super(ithColumn, jthColumn, kthColumn);
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
  public MatrixIJK(double scaleI, UnwritableVectorIJK ithColumn, double scaleJ,
      UnwritableVectorIJK jthColumn, double scaleK, UnwritableVectorIJK kthColumn) {
    super(scaleI, ithColumn, scaleJ, jthColumn, scaleK, kthColumn);
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable subclass rather than the
   * unwritable parent.
   */
  @Override
  public MatrixIJK createTranspose() {
    return new MatrixIJK(this).transpose();
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable subclass rather than the
   * unwritable parent.
   */
  @Override
  public MatrixIJK createUnitizedColumns() {
    return new MatrixIJK(this).unitizeColumns();
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable subclass rather than the
   * unwritable parent.
   */
  @Override
  public MatrixIJK createAdjugate() {
    return new MatrixIJK(this).adjugate();
  }


  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable subclass rather than the
   * unwritable parent.
   */
  @Override
  public MatrixIJK createInverse() {
    return new MatrixIJK(this).invert();
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable subclass rather than the
   * unwritable parent.
   */
  @Override
  public MatrixIJK createInverse(double tolerance) {
    return new MatrixIJK(this).invert(tolerance);
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable subclass rather than the
   * unwritable parent.
   */
  @Override
  public MatrixIJK createInvorted() {
    return new MatrixIJK(this).invort();
  }

  /**
   * Transpose the matrix.
   * 
   * @return a reference to the instance for convenience, which now contains the transpose
   */
  public MatrixIJK transpose() {
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
   * Modifies the elements of this matrix so that each column in the matrix becomes a unit vector
   * 
   * @return a reference to the instance for convenience
   * 
   * @throws UnsupportedOperationException if any of the columns are of length zero
   */
  public MatrixIJK unitizeColumns() {
    setTo(new VectorIJK(this.ii, this.ji, this.ki).unitize(), // first
        // column
        new VectorIJK(this.ij, this.jj, this.kj).unitize(), // second
        // column
        new VectorIJK(this.ik, this.jk, this.kk).unitize()); // third
    // column
    return this;
  }

  /**
   * Replace the matrix with its adjugate (i.e., the transpose of its cofactor matrix). Dividing the
   * resultant matrix by the determinant of the original matrix (assuming it is non-zero) yields the
   * inverse of the original matrix.
   * 
   * @return a reference to the instance for convenience, which now contains the adjunct
   */
  public MatrixIJK adjugate() {

    double cii = (jj * kk - kj * jk);
    double cij = -(ij * kk - kj * ik);
    double cik = (ij * jk - jj * ik);
    double cji = -(ji * kk - ki * jk);
    double cjj = (ii * kk - ki * ik);
    double cjk = -(ii * jk - ji * ik);
    double cki = (ji * kj - ki * jj);
    double ckj = -(ii * kj - ki * ij);
    double ckk = (ii * jj - ji * ij);

    return setTo(cii, cji, cki, cij, cjj, ckj, cik, cjk, ckk);
  }

  /**
   * Invert the matrix if the determinant is not within the default tolerance of zero.
   * 
   * @return a reference to the instance for convenience, which now contains the multiplicative
   *         inverse
   * 
   * @throws UnsupportedOperationException if the determinant of the instance is within
   *         {@link UnwritableMatrixIJK#INVERSION_TOLERANCE} of 0.0.
   */
  public MatrixIJK invert() {
    return invert(UnwritableMatrixIJK.INVERSION_TOLERANCE);
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
  public MatrixIJK invert(double tolerance) {

    double det = getDeterminant();

    if (abs(det) < tolerance) {
      throw new UnsupportedOperationException("Matrix nearly singular, unable to invert.");
    }

    double cii = (jj * kk - kj * jk) / det;
    double cij = -(ij * kk - kj * ik) / det;
    double cik = (ij * jk - jj * ik) / det;
    double cji = -(ji * kk - ki * jk) / det;
    double cjj = (ii * kk - ki * ik) / det;
    double cjk = -(ii * jk - ji * ik) / det;
    double cki = (ji * kj - ki * jj) / det;
    double ckj = -(ii * kj - ki * ij) / det;
    double ckk = (ii * jj - ji * ij) / det;

    return setTo(cii, cji, cki, cij, cjj, ckj, cik, cjk, ckk);
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
  public MatrixIJK invort() {

    transpose();

    double length = computeNorm(this.ii, this.ij, this.ik);

    if ((length * INVORSION_BOUND < 1) || (length == 0)) {
      throw new UnsupportedOperationException(
          "ith column of matrix has length, " + length + ", for which there is no inverse.");
    }

    this.ii /= length;
    this.ii /= length;
    this.ij /= length;
    this.ij /= length;
    this.ik /= length;
    this.ik /= length;

    length = computeNorm(this.ji, this.jj, this.jk);

    if ((length * INVORSION_BOUND < 1) || (length == 0)) {
      throw new UnsupportedOperationException(
          "jth column of matrix has length, " + length + ", for which there is no inverse.");
    }

    this.ji /= length;
    this.ji /= length;
    this.jj /= length;
    this.jj /= length;
    this.jk /= length;
    this.jk /= length;

    length = computeNorm(this.ki, this.kj, this.kk);

    if ((length * INVORSION_BOUND < 1) || (length == 0)) {
      throw new UnsupportedOperationException(
          "kth column of matrix has length, " + length + ", for which there is no inverse.");
    }

    this.ki /= length;
    this.ki /= length;
    this.kj /= length;
    this.kj /= length;
    this.kk /= length;
    this.kk /= length;

    return this;
  }

  /**
   * Scales each component of the matrix by the supplied factor.
   * 
   * @param scale the scale factor to apply
   * 
   * @return a reference to the instance which now contains the scaled matrix.
   */
  public MatrixIJK scale(double scale) {

    this.ii *= scale;
    this.ji *= scale;
    this.ki *= scale;
    this.ij *= scale;
    this.jj *= scale;
    this.kj *= scale;
    this.ik *= scale;
    this.jk *= scale;
    this.kk *= scale;

    return this;
  }

  /**
   * Scales each column of the matrix by the supplied factors.
   * 
   * @param scaleI the ith column scale
   * @param scaleJ the jth column scale
   * @param scaleK the kth column scale
   * 
   * @return a reference to the instance for convenience which contains the scaled matrix.
   */
  public MatrixIJK scale(double scaleI, double scaleJ, double scaleK) {

    this.ii *= scaleI;
    this.ji *= scaleI;
    this.ki *= scaleI;
    this.ij *= scaleJ;
    this.jj *= scaleJ;
    this.kj *= scaleJ;
    this.ik *= scaleK;
    this.jk *= scaleK;
    this.kk *= scaleK;

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
   */
  public final void setJI(double ji) {
    this.ji = ji;
  }

  /**
   * Sets the kth row, ith column component.
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
   */
  public final void setKI(double ki) {
    this.ki = ki;
  }

  /**
   * Sets the ith row, jth column component.
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
   */
  public final void setJJ(double jj) {
    this.jj = jj;
  }

  /**
   * Sets the kth row, jth column component.
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
   * <td><b>kj</b></td>
   * <td>kk</td>
   * </tr>
   * </table>
   * <br>
   * </p>
   */
  public final void setKJ(double kj) {
    this.kj = kj;
  }

  /**
   * Sets the ith row, kth column component.
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
   */
  public final void setIK(double ik) {
    this.ik = ik;
  }

  /**
   * Sets the jth row, kth column component.
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
   */
  public final void setJK(double jk) {
    this.jk = jk;
  }

  /**
   * Sets the kth row, kth column component.
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
   */
  public final void setKK(double kk) {
    this.kk = kk;
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
   * @param value the value to place into the matrix at (row,column).
   * 
   * @throws IllegalArgumentException if either the supplied row or column index are outside their
   *         acceptable ranges of [0,2].
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
          case 2:
            this.ik = value;
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
          case 2:
            this.jk = value;
            return;
          default:
            throw new IllegalArgumentException(
                "Unable to set element (" + row + ", " + column + "). Column index invalid.");

        }

      case 2:
        switch (column) {
          case 0:
            this.ki = value;
            return;
          case 1:
            this.kj = value;
            return;
          case 2:
            this.kk = value;
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
   * <td>ik</td>
   * </tr>
   * <tr>
   * <td><b>ji</b></td>
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
   * @param column the vector whose components are to replace the ith column of this matrix
   */
  public final void setIthColumn(UnwritableVectorIJK column) {
    this.ii = column.i;
    this.ji = column.j;
    this.ki = column.k;
  }

  /**
   * Sets the jth column to the supplied vector.
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
   * <td><b>jj</b></td>
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
   * @param column the vector whose components are to replace the jth column of this matrix
   */
  public final void setJthColumn(UnwritableVectorIJK column) {
    this.ij = column.i;
    this.jj = column.j;
    this.kj = column.k;
  }

  /**
   * Sets the kth column to the supplied vector.
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
   * <td><b>jk</b></td>
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
   * @param column the vector whose components are to replace the kth column of this matrix
   */
  public final void setKthColumn(UnwritableVectorIJK column) {
    this.ik = column.i;
    this.jk = column.j;
    this.kk = column.k;
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
   * <td>2</td>
   * </tr>
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
   * </p>
   * 
   * @param columnIndex a column index in [0,2].
   * @param column the vector whose components are to replace the specified column of this matrix
   * 
   * @throws IllegalArgumentException if the supplied columnIndex is not in [0,2].
   */
  public final void setColumn(int columnIndex, UnwritableVectorIJK column) {

    switch (columnIndex) {
      case 0:
        setIthColumn(column);
        return;
      case 1:
        setJthColumn(column);
        return;
      case 2:
        setKthColumn(column);
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
   * @param ki kth row, ith column element
   * @param ij ith row, jth column element
   * @param jj jth row, jth column element
   * @param kj kth row, jth column element
   * @param ik ith row, kth column element
   * @param jk jth row, kth column element
   * @param kk kth row, kth column element
   * 
   * @return a reference to the instance, for convenience, that contains the newly set matrix
   */
  public final MatrixIJK setTo(double ii, double ji, double ki, double ij, double jj, double kj,
      double ik, double jk, double kk) {
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
   */
  public final MatrixIJK setTo(double[][] data) {
    setTo(data[0][0], data[1][0], data[2][0], data[0][1], data[1][1], data[2][1], data[0][2],
        data[1][2], data[2][2]);
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
  public final MatrixIJK setTo(UnwritableMatrixIJK matrix) {
    setTo(matrix.ii, matrix.ji, matrix.ki, matrix.ij, matrix.jj, matrix.kj, matrix.ik, matrix.jk,
        matrix.kk);
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
  public final MatrixIJK setTo(double scale, UnwritableMatrixIJK matrix) {
    setTo(matrix);
    scale(scale);
    return this;
  }

  /**
   * Sets the contents of this matrix to a column-wise scaled version of the supplied matrix
   * 
   * @param scaleI the scale factor to apply to the ith column of matrix
   * @param scaleJ the scale factor to apply to the jth column of matrix
   * @param scaleK the scale factor to apply to the kth column of matrix
   * @param matrix the matrix to scale
   * 
   * @return a reference to this instance for convenience that contains the column scaled version of
   *         matrix
   */
  public final MatrixIJK setTo(double scaleI, double scaleJ, double scaleK,
      UnwritableMatrixIJK matrix) {
    setTo(matrix);
    scale(scaleI, scaleJ, scaleK);
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
   */
  public final MatrixIJK setTo(UnwritableVectorIJK ithColumn, UnwritableVectorIJK jthColumn,
      UnwritableVectorIJK kthColumn) {
    setTo(ithColumn.i, ithColumn.j, ithColumn.k, jthColumn.i, jthColumn.j, jthColumn.k, kthColumn.i,
        kthColumn.j, kthColumn.k);
    return this;
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
   */
  public final MatrixIJK setTo(double scaleI, UnwritableVectorIJK ithColumn, double scaleJ,
      UnwritableVectorIJK jthColumn, double scaleK, UnwritableVectorIJK kthColumn) {
    setTo(scaleI * ithColumn.i, scaleI * ithColumn.j, scaleI * ithColumn.k, scaleJ * jthColumn.i,
        scaleJ * jthColumn.j, scaleJ * jthColumn.k, scaleK * kthColumn.i, scaleK * kthColumn.j,
        scaleK * kthColumn.k);
    return this;
  }

  /**
   * Sets this matrix components to the transpose of the supplied matrix.
   * 
   * @param matrix the matrix whose transpose is to be copied into the instance
   * 
   * @return a reference to the instance for convenience
   */
  public final MatrixIJK setToTranspose(UnwritableMatrixIJK matrix) {
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
  public final MatrixIJK setToUnitizedColumns(MatrixIJK matrix) {
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
   *         {@link UnwritableMatrixIJK#INVERSION_TOLERANCE} of 0.0.
   */
  public final MatrixIJK setToInverse(UnwritableMatrixIJK matrix) {

    double det = matrix.getDeterminant();

    if (abs(det) < UnwritableMatrixIJK.DETERMINANT_TOLERANCE) {
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
  public final MatrixIJK setToInverse(UnwritableMatrixIJK matrix, double tolerance) {

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
  public final MatrixIJK setToInvorted(UnwritableMatrixIJK matrix) {
    return setTo(matrix).invort();
  }

  /**
   * Compute the product of a matrix with the transpose of another matrix.
   * 
   * @param a the left hand matrix
   * @param b the right hand matrix to transpose, then multiply
   * 
   * @return a new <code>MatrixIJK</code> containing the product.
   * 
   * @see MatrixIJK#mxmt(UnwritableMatrixIJK, UnwritableMatrixIJK, MatrixIJK)
   */
  public static MatrixIJK mxmt(UnwritableMatrixIJK a, UnwritableMatrixIJK b) {
    return mxmt(a, b, new MatrixIJK());
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
  public static MatrixIJK mxmt(UnwritableMatrixIJK a, UnwritableMatrixIJK b, MatrixIJK buffer) {
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
   * Compute the product of a transpose of a matrix with another matrix.
   * 
   * @param a the left hand matrix to transpose, then multiply
   * @param b the right hand matrix
   * 
   * @return a new <code>MatrixIJK</code> containing the product
   * 
   * @see MatrixIJK#mtxm(UnwritableMatrixIJK, UnwritableMatrixIJK, MatrixIJK)
   */
  public static MatrixIJK mtxm(UnwritableMatrixIJK a, UnwritableMatrixIJK b) {
    return mtxm(a, b, new MatrixIJK());
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
  public static MatrixIJK mtxm(UnwritableMatrixIJK a, UnwritableMatrixIJK b, MatrixIJK buffer) {
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
   * Compute the product of two matrices.
   * 
   * @param a the left hand matrix
   * @param b the right hand matrix
   * 
   * @return a new <code>MatrixIJK</code> containing the product (ab).
   * 
   * @see MatrixIJK#mxm(UnwritableMatrixIJK, UnwritableMatrixIJK, MatrixIJK)
   */
  public static MatrixIJK mxm(UnwritableMatrixIJK a, UnwritableMatrixIJK b) {
    return mxm(a, b, new MatrixIJK());
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
  public static MatrixIJK mxm(UnwritableMatrixIJK a, UnwritableMatrixIJK b, MatrixIJK buffer) {
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
   * Compute the sum of a pair of matrices multipled with another matrix transposed
   * 
   * @param a left hand matrix in the first product
   * @param b right hand matrix to transpose in the first product
   * @param c left hand matrix in the second product
   * @param d right hand matrix to transpose in the second product
   * 
   * @return a new <code>MatrixIJK</code> containing (a x bt) + (c x dt)
   * 
   * @see MatrixIJK#mxmtadd(UnwritableMatrixIJK, UnwritableMatrixIJK, UnwritableMatrixIJK,
   *      UnwritableMatrixIJK, MatrixIJK)
   */
  public static MatrixIJK mxmtadd(UnwritableMatrixIJK a, UnwritableMatrixIJK b,
      UnwritableMatrixIJK c, UnwritableMatrixIJK d) {
    return mxmtadd(a, b, c, d, new MatrixIJK());
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
  public static MatrixIJK mxmtadd(UnwritableMatrixIJK a, UnwritableMatrixIJK b,
      UnwritableMatrixIJK c, UnwritableMatrixIJK d, MatrixIJK buffer) {

    double ii = a.ii * b.ii + a.ij * b.ij + a.ik * b.ik;
    double ij = a.ii * b.ji + a.ij * b.jj + a.ik * b.jk;
    double ik = a.ii * b.ki + a.ij * b.kj + a.ik * b.kk;

    double ji = a.ji * b.ii + a.jj * b.ij + a.jk * b.ik;
    double jj = a.ji * b.ji + a.jj * b.jj + a.jk * b.jk;
    double jk = a.ji * b.ki + a.jj * b.kj + a.jk * b.kk;

    double ki = a.ki * b.ii + a.kj * b.ij + a.kk * b.ik;
    double kj = a.ki * b.ji + a.kj * b.jj + a.kk * b.jk;
    double kk = a.ki * b.ki + a.kj * b.kj + a.kk * b.kk;

    ii += c.ii * d.ii + c.ij * d.ij + c.ik * d.ik;
    ij += c.ii * d.ji + c.ij * d.jj + c.ik * d.jk;
    ik += c.ii * d.ki + c.ij * d.kj + c.ik * d.kk;

    ji += c.ji * d.ii + c.jj * d.ij + c.jk * d.ik;
    jj += c.ji * d.ji + c.jj * d.jj + c.jk * d.jk;
    jk += c.ji * d.ki + c.jj * d.kj + c.jk * d.kk;

    ki += c.ki * d.ii + c.kj * d.ij + c.kk * d.ik;
    kj += c.ki * d.ji + c.kj * d.jj + c.kk * d.jk;
    kk += c.ki * d.ki + c.kj * d.kj + c.kk * d.kk;

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
   * Compute the sum of a pair of matrix transposes multipled with another matrix
   * 
   * @param a left hand matrix to transpose in the first product
   * @param b right hand matrix in the first product
   * @param c left hand matrix to transpose in the second product
   * @param d right hand matrix in the second product
   * 
   * @return a new <code>MatrixIJK</code> containing (at x b) + (ct x d)
   * 
   * @see MatrixIJK#mtxmadd(UnwritableMatrixIJK, UnwritableMatrixIJK, UnwritableMatrixIJK,
   *      UnwritableMatrixIJK, MatrixIJK)
   */
  public static MatrixIJK mtxmadd(UnwritableMatrixIJK a, UnwritableMatrixIJK b,
      UnwritableMatrixIJK c, UnwritableMatrixIJK d) {
    return mtxmadd(a, b, c, d, new MatrixIJK());
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
  public static MatrixIJK mtxmadd(UnwritableMatrixIJK a, UnwritableMatrixIJK b,
      UnwritableMatrixIJK c, UnwritableMatrixIJK d, MatrixIJK buffer) {

    double ii = a.ii * b.ii + a.ji * b.ji + a.ki * b.ki;
    double ij = a.ii * b.ij + a.ji * b.jj + a.ki * b.kj;
    double ik = a.ii * b.ik + a.ji * b.jk + a.ki * b.kk;

    double ji = a.ij * b.ii + a.jj * b.ji + a.kj * b.ki;
    double jj = a.ij * b.ij + a.jj * b.jj + a.kj * b.kj;
    double jk = a.ij * b.ik + a.jj * b.jk + a.kj * b.kk;

    double ki = a.ik * b.ii + a.jk * b.ji + a.kk * b.ki;
    double kj = a.ik * b.ij + a.jk * b.jj + a.kk * b.kj;
    double kk = a.ik * b.ik + a.jk * b.jk + a.kk * b.kk;

    ii += c.ii * d.ii + c.ji * d.ji + c.ki * d.ki;
    ij += c.ii * d.ij + c.ji * d.jj + c.ki * d.kj;
    ik += c.ii * d.ik + c.ji * d.jk + c.ki * d.kk;

    ji += c.ij * d.ii + c.jj * d.ji + c.kj * d.ki;
    jj += c.ij * d.ij + c.jj * d.jj + c.kj * d.kj;
    jk += c.ij * d.ik + c.jj * d.jk + c.kj * d.kk;

    ki += c.ik * d.ii + c.jk * d.ji + c.kk * d.ki;
    kj += c.ik * d.ij + c.jk * d.jj + c.kk * d.kj;
    kk += c.ik * d.ik + c.jk * d.jk + c.kk * d.kk;

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
   * Compute the sum of the products of two pairs of matrices.
   * 
   * @param a left hand matrix in first product
   * @param b right hand matrix in first product
   * @param c left hand matrix in second product
   * @param d right hand matrix in second product
   * 
   * @return a new <code>MatrixIJK</code> containing (a x b) + (c x d)
   * 
   * @see MatrixIJK#mxmadd(UnwritableMatrixIJK, UnwritableMatrixIJK, UnwritableMatrixIJK,
   *      UnwritableMatrixIJK, MatrixIJK)
   */
  public static MatrixIJK mxmadd(UnwritableMatrixIJK a, UnwritableMatrixIJK b,
      UnwritableMatrixIJK c, UnwritableMatrixIJK d) {
    return mxmadd(a, b, c, d, new MatrixIJK());
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
  public static MatrixIJK mxmadd(UnwritableMatrixIJK a, UnwritableMatrixIJK b,
      UnwritableMatrixIJK c, UnwritableMatrixIJK d, MatrixIJK buffer) {

    double ii = a.ii * b.ii + a.ij * b.ji + a.ik * b.ki;
    double ij = a.ii * b.ij + a.ij * b.jj + a.ik * b.kj;
    double ik = a.ii * b.ik + a.ij * b.jk + a.ik * b.kk;

    double ji = a.ji * b.ii + a.jj * b.ji + a.jk * b.ki;
    double jj = a.ji * b.ij + a.jj * b.jj + a.jk * b.kj;
    double jk = a.ji * b.ik + a.jj * b.jk + a.jk * b.kk;

    double ki = a.ki * b.ii + a.kj * b.ji + a.kk * b.ki;
    double kj = a.ki * b.ij + a.kj * b.jj + a.kk * b.kj;
    double kk = a.ki * b.ik + a.kj * b.jk + a.kk * b.kk;

    ii += c.ii * d.ii + c.ij * d.ji + c.ik * d.ki;
    ij += c.ii * d.ij + c.ij * d.jj + c.ik * d.kj;
    ik += c.ii * d.ik + c.ij * d.jk + c.ik * d.kk;

    ji += c.ji * d.ii + c.jj * d.ji + c.jk * d.ki;
    jj += c.ji * d.ij + c.jj * d.jj + c.jk * d.kj;
    jk += c.ji * d.ik + c.jj * d.jk + c.jk * d.kk;

    ki += c.ki * d.ii + c.kj * d.ji + c.kk * d.ki;
    kj += c.ki * d.ij + c.kj * d.jj + c.kk * d.kj;
    kk += c.ki * d.ik + c.kj * d.jk + c.kk * d.kk;

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
   * Compute the component-wise difference of two matrices.
   * 
   * @param a the minuend matrix
   * @param b the subtrahend matrix
   * 
   * @return a new <code>MatrixIJK</code> which contains (a - b)
   * 
   * @see MatrixIJK#subtract(UnwritableMatrixIJK, UnwritableMatrixIJK, MatrixIJK)
   */
  public static MatrixIJK subtract(UnwritableMatrixIJK a, UnwritableMatrixIJK b) {
    return subtract(a, b, new MatrixIJK());
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
  public static MatrixIJK subtract(UnwritableMatrixIJK a, UnwritableMatrixIJK b, MatrixIJK buffer) {
    buffer.ii = a.ii - b.ii;
    buffer.ji = a.ji - b.ji;
    buffer.ki = a.ki - b.ki;
    buffer.ij = a.ij - b.ij;
    buffer.jj = a.jj - b.jj;
    buffer.kj = a.kj - b.kj;
    buffer.ik = a.ik - b.ik;
    buffer.jk = a.jk - b.jk;
    buffer.kk = a.kk - b.kk;

    return buffer;
  }

  /**
   * Compute component-wise sum of two matrices.
   * 
   * @param a a matrix
   * @param b another matrix
   * 
   * @return a new <code>MatrixIJK</code> containing (a + b)
   * 
   * @see MatrixIJK#add(UnwritableMatrixIJK, UnwritableMatrixIJK, MatrixIJK)
   */
  public static MatrixIJK add(UnwritableMatrixIJK a, UnwritableMatrixIJK b) {
    return add(a, b, new MatrixIJK());
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
  public static MatrixIJK add(UnwritableMatrixIJK a, UnwritableMatrixIJK b, MatrixIJK buffer) {
    buffer.ii = a.ii + b.ii;
    buffer.ji = a.ji + b.ji;
    buffer.ki = a.ki + b.ki;
    buffer.ij = a.ij + b.ij;
    buffer.jj = a.jj + b.jj;
    buffer.kj = a.kj + b.kj;
    buffer.ik = a.ik + b.ik;
    buffer.jk = a.jk + b.jk;
    buffer.kk = a.kk + b.kk;

    return buffer;
  }

  /**
   * Compute the product of the transpose of a matrix with a vector.
   * 
   * @param m the matrix
   * @param v the vector
   * 
   * @return a new <code>VectorIJK</code> containing the result.
   * 
   * @see UnwritableMatrixIJK#mtxv(UnwritableVectorIJK)
   */
  @Deprecated
  public static VectorIJK mtxv(UnwritableMatrixIJK m, UnwritableVectorIJK v) {
    return m.mtxv(v, new VectorIJK());
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
   * @see UnwritableMatrixIJK#mtxv(UnwritableVectorIJK, VectorIJK)
   */
  @Deprecated
  public static VectorIJK mtxv(UnwritableMatrixIJK m, UnwritableVectorIJK v, VectorIJK buffer) {
    return m.mtxv(v, buffer);
  }

  /**
   * Compute the product of a matrix with a vector.
   * 
   * @param m the matrix
   * @param v the vector
   * 
   * @return a new <code>VectorIJK</code> containing the result.
   * 
   * @see UnwritableMatrixIJK#mxv(UnwritableVectorIJK)
   */
  @Deprecated
  public static VectorIJK mxv(UnwritableMatrixIJK m, UnwritableVectorIJK v) {
    return m.mxv(v, new VectorIJK());
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
   * @see UnwritableMatrixIJK#mxv(UnwritableVectorIJK, VectorIJK)
   */
  @Deprecated
  public static VectorIJK mxv(UnwritableMatrixIJK m, UnwritableVectorIJK v, VectorIJK buffer) {
    return m.mxv(v, buffer);
  }

}
