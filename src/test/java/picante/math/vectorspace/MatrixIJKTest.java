package picante.math.vectorspace;

import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEqualDouble;
import static picante.junit.AssertTools.assertEqualMatrix;
import static picante.junit.AssertTools.assertEqualVector;
import static picante.junit.AssertTools.assertEquivalentMatrix;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/*
 * TODO: Modify inversion test routines that take a threshold to check threshold is implemented
 * properly.
 */
public class MatrixIJKTest {

  private static final double LOOSE_TOLERANCE = 1.0E-14;

  private MatrixIJK defaultCon;
  private MatrixIJK valueCon;
  private MatrixIJK arrayCon;
  private MatrixIJK copyCon;
  private MatrixIJK scaleCon;
  private MatrixIJK vectorCon;
  private MatrixIJK scaleVectorCon;

  private double[][] array;
  private UnwritableVectorIJK ithCol;
  private UnwritableVectorIJK jthCol;
  private UnwritableVectorIJK kthCol;
  private UnwritableVectorIJK ortho1;
  private UnwritableVectorIJK ortho2;
  private UnwritableVectorIJK ortho3;

  private VectorIJK a;
  private VectorIJK b;

  private MatrixIJK k;
  private MatrixIJK l;
  private MatrixIJK m;
  private MatrixIJK n;
  private MatrixIJK o;

  @Before
  public void setUp() throws Exception {
    array = new double[][] {{1, -2, 3}, {-4, 5, -6}, {7, -8, 9}};
    ithCol = new UnwritableVectorIJK(1, -2, 3);
    jthCol = new UnwritableVectorIJK(-5, 7, -11);
    kthCol = new UnwritableVectorIJK(13, -17, 19);

    defaultCon = new MatrixIJK();
    valueCon = new MatrixIJK(-1, 2, -3, 4, -5, 6, -7, 8, -9);
    arrayCon = new MatrixIJK(array);
    copyCon = new MatrixIJK(valueCon);
    scaleCon = new MatrixIJK(-2.0, valueCon);
    vectorCon = new MatrixIJK(ithCol, jthCol, kthCol);
    scaleVectorCon = new MatrixIJK(3.0, ithCol, 2.0, jthCol, -1.0, kthCol);

    ortho1 = new UnwritableVectorIJK(1, 2, 1).createUnitized();
    ortho2 = VectorIJK.cross(ortho1, new UnwritableVectorIJK(5, 1, 1).createUnitized());
    ortho3 = VectorIJK.cross(ortho1, ortho2).createUnitized();

    a = new VectorIJK(1, 2, 3);
    b = new VectorIJK(4, 5, 6);

    k = new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34);
    l = new MatrixIJK(1, 0, -1, 1, -1, 1, 2, -3, 4);
    m = new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9);
    n = new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3);
    o = new MatrixIJK();

  }

  @After
  public void tearDown() throws Exception {
    checkStaticFinalMembers();
  }

  @Test
  public void testMatrixIJK() {
    assertEqualMatrix(new UnwritableMatrixIJK(1, 0, 0, 0, 1, 0, 0, 0, 1), defaultCon);
  }

  @Test
  public void testMatrixIJKDoubleDoubleDoubleDoubleDoubleDoubleDoubleDoubleDouble() {
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, 4, -5, 6, -7, 8, -9), valueCon);
  }

  @Test
  public void testMatrixIJKDoubleUnwritableMatrixIJK() {
    assertEqualMatrix(new UnwritableMatrixIJK(2, -4, 6, -8, 10, -12, 14, -16, 18), scaleCon);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, 4, -5, 6, -7, 8, -9), valueCon);
  }

  @Test
  public void testMatrixIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJK() {
    assertEqualMatrix(new UnwritableMatrixIJK(3, -6, 9, -10, 14, -22, -13, 17, -19),
        scaleVectorCon);
    assertEqualVector(new UnwritableVectorIJK(1, -2, 3), ithCol);
    assertEqualVector(new UnwritableVectorIJK(-5, 7, -11), jthCol);
    assertEqualVector(new UnwritableVectorIJK(13, -17, 19), kthCol);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testMatrixIJKDoubleArrayArrayException() {
    double[][] badArray = {{1, 2}, {3, 4}, {5, 6}};
    new MatrixIJK(badArray);
  }

  @Test
  public void testMatrixIJKDoubleArrayArray() {
    assertEqualMatrix(new UnwritableMatrixIJK(1, -4, 7, -2, 5, -8, 3, -6, 9), arrayCon);
    assertEqualDouble(1, array[0][0]);
    assertEqualDouble(-2, array[0][1]);
    assertEqualDouble(3, array[0][2]);
    assertEqualDouble(-4, array[1][0]);
    assertEqualDouble(5, array[1][1]);
    assertEqualDouble(-6, array[1][2]);
    assertEqualDouble(7, array[2][0]);
    assertEqualDouble(-8, array[2][1]);
    assertEqualDouble(9, array[2][2]);
  }

  @Test
  public void testMatrixIJKUnwritableMatrixIJK() {
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, 4, -5, 6, -7, 8, -9), copyCon);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, 4, -5, 6, -7, 8, -9), valueCon);
  }

  @Test
  public void testMatrixIJKUnwritableVectorIJKUnwritableVectorIJKUnwritableVectorIJK() {
    assertEqualMatrix(new UnwritableMatrixIJK(1, -2, 3, -5, 7, -11, 13, -17, 19), vectorCon);

    assertEqualVector(new UnwritableVectorIJK(1, -2, 3), ithCol);
    assertEqualVector(new UnwritableVectorIJK(-5, 7, -11), jthCol);
    assertEqualVector(new UnwritableVectorIJK(13, -17, 19), kthCol);

  }

  @Test
  public void testCreateTranspose() {
    MatrixIJK transpose = valueCon.createTranspose();
    assertNotSame(transpose, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 4, -7, 2, -5, 8, -3, 6, -9), transpose);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, 4, -5, 6, -7, 8, -9), valueCon);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInverseException() {
    valueCon.createInverse();
  }

  @Test
  public void testCreateInverse() {
    MatrixIJK inverse = vectorCon.createInverse();
    assertNotSame(inverse, vectorCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, -2, 3, -5, 7, -11, 13, -17, 19), vectorCon);
    UnwritableMatrixIJK product = MatrixIJK.mxm(inverse, vectorCon, new MatrixIJK());
    assertComponentEquals(MatrixIJK.IDENTITY, product, LOOSE_TOLERANCE);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInverseDoubleException() {
    valueCon.createInverse(0.0001);
  }

  @Test
  public void testCreateInverseDouble() {
    MatrixIJK inverse = vectorCon.createInverse(0.0001);
    assertNotSame(inverse, vectorCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, -2, 3, -5, 7, -11, 13, -17, 19), vectorCon);
    MatrixIJK product = MatrixIJK.mxm(inverse, vectorCon, new MatrixIJK());
    assertComponentEquals(MatrixIJK.IDENTITY, product, LOOSE_TOLERANCE);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInvortedIthColumnException() {
    new MatrixIJK(1E-312, ortho2, 1E30, ortho3, 10, ortho1).createInvorted();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInvortedJthColumnException() {
    new MatrixIJK(1E30, ortho2, 1E-312, ortho3, 10, ortho1).createInvorted();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInvortedKthColumnException() {
    new MatrixIJK(1E30, ortho2, 10, ortho3, 1E-312, ortho1).createInvorted();
  }

  @Test
  public void testCreateInvorted() {

    MatrixIJK m = new MatrixIJK(5, ortho1, 3.1E30, ortho2, 8.7E-50, ortho3);
    MatrixIJK inverseM = m.createInvorted();
    assertNotSame(m, inverseM);
    assertEqualMatrix(new MatrixIJK(5, ortho1, 3.1E30, ortho2, 8.7E-50, ortho3), m);
    assertComponentEquals(MatrixIJK.IDENTITY, MatrixIJK.mxm(m, inverseM), LOOSE_TOLERANCE);

  }

  @Test
  public void testTranspose() {
    MatrixIJK transpose = valueCon.transpose();
    assertSame(transpose, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 4, -7, 2, -5, 8, -3, 6, -9), valueCon);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testInvertException() {
    valueCon.invert();
  }

  @Test
  public void testInvert() {
    MatrixIJK copy = new MatrixIJK(vectorCon);
    MatrixIJK inverse = vectorCon.invert();
    assertSame(inverse, vectorCon);
    MatrixIJK product = MatrixIJK.mxm(inverse, copy, new MatrixIJK());
    assertComponentEquals(MatrixIJK.IDENTITY, product, LOOSE_TOLERANCE);

  }

  @Test(expected = UnsupportedOperationException.class)
  public void testInvertDoubleException() {
    valueCon.invert(0.0001);
  }

  @Test
  public void testInvertDouble() {
    MatrixIJK copy = new MatrixIJK(vectorCon);
    MatrixIJK inverse = vectorCon.invert(0.00001);
    assertSame(inverse, vectorCon);
    MatrixIJK product = MatrixIJK.mxm(inverse, copy, new MatrixIJK());
    assertComponentEquals(MatrixIJK.IDENTITY, product, LOOSE_TOLERANCE);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testInvortIthColumnException() {
    new MatrixIJK(1E-312, ortho2, 1E30, ortho3, 10, ortho1).invort();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testInvortJthColumnException() {
    new MatrixIJK(1E30, ortho2, 1E-312, ortho3, 10, ortho1).invort();

  }

  @Test(expected = UnsupportedOperationException.class)
  public void testInvortKthColumnException() {
    new MatrixIJK(1E30, ortho2, 10, ortho3, 1E-312, ortho1).invort();
  }

  @Test
  public void testInvort() {

    MatrixIJK toInvort = new MatrixIJK(50, ortho1, 12, ortho2, 5000, ortho3);

    MatrixIJK result = toInvort.invort();

    assertSame(result, toInvort);
    assertComponentEquals(MatrixIJK.IDENTITY,
        MatrixIJK.mxm(result, new MatrixIJK(50, ortho1, 12, ortho2, 5000, ortho3)),
        LOOSE_TOLERANCE);

  }

  @Test
  public void testScaleDouble() {
    MatrixIJK scale = valueCon.scale(2.0);
    assertSame(scale, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(-2, 4, -6, 8, -10, 12, -14, 16, -18), valueCon);

  }

  @Test
  public void testScaleDoubleDoubleDouble() {
    MatrixIJK scale = valueCon.scale(-1, 2, 3);
    assertSame(scale, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, -2, 3, 8, -10, 12, -21, 24, -27), valueCon);
  }

  @Test
  public void testSetII() {
    valueCon.setII(Math.PI);
    assertEqualMatrix(new UnwritableMatrixIJK(Math.PI, 2, -3, 4, -5, 6, -7, 8, -9), valueCon);
  }

  @Test
  public void testSetJI() {
    valueCon.setJI(Math.PI);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, Math.PI, -3, 4, -5, 6, -7, 8, -9), valueCon);
  }

  @Test
  public void testSetKI() {
    valueCon.setKI(Math.PI);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, Math.PI, 4, -5, 6, -7, 8, -9), valueCon);
  }

  @Test
  public void testSetIJ() {
    valueCon.setIJ(Math.PI);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, Math.PI, -5, 6, -7, 8, -9), valueCon);
  }

  @Test
  public void testSetJJ() {
    valueCon.setJJ(Math.PI);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, 4, Math.PI, 6, -7, 8, -9), valueCon);
  }

  @Test
  public void testSetKJ() {
    valueCon.setKJ(Math.PI);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, 4, -5, Math.PI, -7, 8, -9), valueCon);
  }

  @Test
  public void testSetIK() {
    valueCon.setIK(Math.PI);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, 4, -5, 6, Math.PI, 8, -9), valueCon);
  }

  @Test
  public void testSetJK() {
    valueCon.setJK(Math.PI);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, 4, -5, 6, -7, Math.PI, -9), valueCon);
  }

  @Test
  public void testSetKK() {
    valueCon.setKK(Math.PI);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, 4, -5, 6, -7, 8, Math.PI), valueCon);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSetIllegalRowException() {
    valueCon.set(4, 0, 20);
  }

  /*
   * This could be more exhaustive, given the implementation with a case statement, but I think it's
   * sufficient for now. (FST)
   */
  @Test(expected = IllegalArgumentException.class)
  public void testSetIllegalColumnException() {
    valueCon.set(0, 4, 20);
  }

  @Test
  public void testSet() {
    valueCon.set(0, 0, 50);
    assertEqualMatrix(new UnwritableMatrixIJK(50, 2, -3, 4, -5, 6, -7, 8, -9), valueCon);

    valueCon.set(1, 0, 51);
    assertEqualMatrix(new UnwritableMatrixIJK(50, 51, -3, 4, -5, 6, -7, 8, -9), valueCon);

    valueCon.set(2, 0, 52);
    assertEqualMatrix(new UnwritableMatrixIJK(50, 51, 52, 4, -5, 6, -7, 8, -9), valueCon);

    valueCon.set(0, 1, 53);
    assertEqualMatrix(new UnwritableMatrixIJK(50, 51, 52, 53, -5, 6, -7, 8, -9), valueCon);

    valueCon.set(1, 1, 54);
    assertEqualMatrix(new UnwritableMatrixIJK(50, 51, 52, 53, 54, 6, -7, 8, -9), valueCon);

    valueCon.set(2, 1, 55);
    assertEqualMatrix(new UnwritableMatrixIJK(50, 51, 52, 53, 54, 55, -7, 8, -9), valueCon);

    valueCon.set(0, 2, 56);
    assertEqualMatrix(new UnwritableMatrixIJK(50, 51, 52, 53, 54, 55, 56, 8, -9), valueCon);

    valueCon.set(1, 2, 57);
    assertEqualMatrix(new UnwritableMatrixIJK(50, 51, 52, 53, 54, 55, 56, 57, -9), valueCon);

    valueCon.set(2, 2, 58);
    assertEqualMatrix(new UnwritableMatrixIJK(50, 51, 52, 53, 54, 55, 56, 57, 58), valueCon);
  }

  @Test
  public void testSetIthColumn() {
    UnwritableVectorIJK column = new UnwritableVectorIJK(10, 11, 12);
    valueCon.setIthColumn(column);
    assertEqualMatrix(new UnwritableMatrixIJK(10, 11, 12, 4, -5, 6, -7, 8, -9), valueCon);
    assertEqualVector(new UnwritableVectorIJK(10, 11, 12), column);
  }

  @Test
  public void testSetJthColumn() {
    UnwritableVectorIJK column = new UnwritableVectorIJK(10, 11, 12);
    valueCon.setJthColumn(column);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, 10, 11, 12, -7, 8, -9), valueCon);
    assertEqualVector(new UnwritableVectorIJK(10, 11, 12), column);
  }

  @Test
  public void testSetKthColumn() {
    UnwritableVectorIJK column = new UnwritableVectorIJK(10, 11, 12);
    valueCon.setKthColumn(column);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, 4, -5, 6, 10, 11, 12), valueCon);
    assertEqualVector(new UnwritableVectorIJK(10, 11, 12), column);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSetColumnException() {
    valueCon.setColumn(-1, new VectorIJK());
  }

  @Test
  public void testSetColumn() {
    UnwritableVectorIJK column = new UnwritableVectorIJK(10, 11, 12);
    valueCon.setColumn(0, column);
    assertEqualMatrix(new UnwritableMatrixIJK(10, 11, 12, 4, -5, 6, -7, 8, -9), valueCon);
    assertEqualVector(new UnwritableVectorIJK(10, 11, 12), column);

    column = new UnwritableVectorIJK(13, 14, 15);
    valueCon.setColumn(1, column);
    assertEqualMatrix(new UnwritableMatrixIJK(10, 11, 12, 13, 14, 15, -7, 8, -9), valueCon);
    assertEqualVector(new UnwritableVectorIJK(13, 14, 15), column);

    column = new UnwritableVectorIJK(16, 17, 18);
    valueCon.setColumn(2, column);
    assertEqualMatrix(new UnwritableMatrixIJK(10, 11, 12, 13, 14, 15, 16, 17, 18), valueCon);
    assertEqualVector(new UnwritableVectorIJK(16, 17, 18), column);

  }

  @Test
  public void testSetToDoubleDoubleDoubleDoubleDoubleDoubleDoubleDoubleDouble() {
    MatrixIJK result = valueCon.setTo(50, 51, 52, 53, 54, 55, 56, 57, 58);
    assertSame(result, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(50, 51, 52, 53, 54, 55, 56, 57, 58), valueCon);
  }

  @Test
  public void testSetToUnwritableVectorIJKUnwritableVectorIJKUnwritableVectorIJK() {
    UnwritableVectorIJK i = new UnwritableVectorIJK(50, 51, 52);
    UnwritableVectorIJK j = new UnwritableVectorIJK(53, 54, 55);
    UnwritableVectorIJK k = new UnwritableVectorIJK(56, 57, 58);
    MatrixIJK result = valueCon.setTo(i, j, k);
    assertSame(result, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(50, 51, 52, 53, 54, 55, 56, 57, 58), valueCon);
    assertEqualVector(new UnwritableVectorIJK(50, 51, 52), i);
    assertEqualVector(new UnwritableVectorIJK(53, 54, 55), j);
    assertEqualVector(new UnwritableVectorIJK(56, 57, 58), k);
  }

  @Test
  public void testSetToDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJK() {
    UnwritableVectorIJK i = new UnwritableVectorIJK(1, 2, 3);
    UnwritableVectorIJK j = new UnwritableVectorIJK(5, 7, 11);
    UnwritableVectorIJK k = new UnwritableVectorIJK(13, 17, 19);
    MatrixIJK result = valueCon.setTo(2, i, 3, j, -1, k);
    assertSame(result, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(2, 4, 6, 15, 21, 33, -13, -17, -19), valueCon);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), i);
    assertEqualVector(new UnwritableVectorIJK(5, 7, 11), j);
    assertEqualVector(new UnwritableVectorIJK(13, 17, 19), k);
  }

  @Test
  public void testSetToUnwritableMatrixIJK() {
    MatrixIJK matrix = valueCon.setTo(arrayCon);
    assertSame(matrix, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, -4, 7, -2, 5, -8, 3, -6, 9), valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, -4, 7, -2, 5, -8, 3, -6, 9), arrayCon);

  }

  @Test
  public void testSetToDoubleUnwritableMatrixIJK() {
    MatrixIJK result = valueCon.setTo(2.0, vectorCon);
    assertSame(result, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(2, -4, 6, -10, 14, -22, 26, -34, 38), valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, -2, 3, -5, 7, -11, 13, -17, 19), vectorCon);
  }

  @Test
  public void testSetToDoubleDoubleDoubleUnwritableMatrixIJK() {
    MatrixIJK result = valueCon.setTo(2.0, 3.0, -1.0, arrayCon);
    assertSame(result, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(2.0, -8.0, 14.0, -6.0, 15.0, -24.0, -3.0, 6.0, -9.0),
        valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, -4, 7, -2, 5, -8, 3, -6, 9), arrayCon);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testSetToDoubleArrayArrayException() {
    double[][] badArray = {{1, 2}, {3, 4}, {5, 6}};
    valueCon.setTo(badArray);
  }

  @Test
  public void testSetToDoubleArrayArray() {

    assertEqualMatrix(new UnwritableMatrixIJK(1, -4, 7, -2, 5, -8, 3, -6, 9), arrayCon);
    assertEqualDouble(1, array[0][0]);
    assertEqualDouble(-2, array[0][1]);
    assertEqualDouble(3, array[0][2]);
    assertEqualDouble(-4, array[1][0]);
    assertEqualDouble(5, array[1][1]);
    assertEqualDouble(-6, array[1][2]);
    assertEqualDouble(7, array[2][0]);
    assertEqualDouble(-8, array[2][1]);
    assertEqualDouble(9, array[2][2]);
  }

  @Test
  public void testSetToTranspose() {
    MatrixIJK transpose = valueCon.setToTranspose(arrayCon);
    assertSame(transpose, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, -2, 3, -4, 5, -6, 7, -8, 9), valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, -4, 7, -2, 5, -8, 3, -6, 9), arrayCon);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testSetToInverseUnwritableMatrixIJKException() {
    vectorCon.setToInverse(valueCon);
  }

  @Test
  public void testSetToInverseUnwritableMatrixIJKExceptionNoChange() {

    try {
      vectorCon.setToInverse(valueCon);
      fail("Expected exception was not thrown.");
    } catch (IllegalArgumentException e) {
      assertEqualMatrix(new UnwritableMatrixIJK(1, -2, 3, -5, 7, -11, 13, -17, 19), vectorCon);
      assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, 4, -5, 6, -7, 8, -9), valueCon);
    }

  }

  @Test
  public void testSetToInverseUnwritableMatrixIJK() {
    MatrixIJK inverse = valueCon.setToInverse(vectorCon);
    assertSame(inverse, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, -2, 3, -5, 7, -11, 13, -17, 19), vectorCon);
    MatrixIJK product = MatrixIJK.mxm(inverse, vectorCon, new MatrixIJK());
    assertComponentEquals(MatrixIJK.IDENTITY, product, LOOSE_TOLERANCE);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSetToInverseUnwritableMatrixIJKDoubleException() {
    vectorCon.setToInverse(valueCon, 0.0001);
  }

  @Test
  public void testSetToInverseUnwritableMatrixIJKDoubleExceptionNoChange() {

    try {
      vectorCon.setToInverse(valueCon, 0.0001);
      fail("Expected exception was not thrown.");
    } catch (IllegalArgumentException e) {
      assertEqualMatrix(new UnwritableMatrixIJK(1, -2, 3, -5, 7, -11, 13, -17, 19), vectorCon);
      assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, 4, -5, 6, -7, 8, -9), valueCon);
    }

  }

  @Test
  public void testSetToInverseUnwritableMatrixIJKDouble() {

    MatrixIJK inverse = valueCon.setToInverse(vectorCon, 0.0001);
    assertSame(inverse, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, -2, 3, -5, 7, -11, 13, -17, 19), vectorCon);
    MatrixIJK product = MatrixIJK.mxm(inverse, vectorCon, new MatrixIJK());
    assertComponentEquals(MatrixIJK.IDENTITY, product, LOOSE_TOLERANCE);

  }

  @Test(expected = UnsupportedOperationException.class)
  public void testSetInvortedIthColumnException() {
    new MatrixIJK().setToInvorted(new MatrixIJK(1E-312, ortho2, 1E30, ortho3, 10, ortho1));
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testSetInvortedJthColumnException() {
    new MatrixIJK().setToInvorted(new MatrixIJK(1E30, ortho2, 1E-312, ortho3, 10, ortho1));
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testSetInvortedKthColumnException() {
    new MatrixIJK().setToInvorted(new MatrixIJK(1E30, ortho2, 10, ortho3, 1E-312, ortho1));
  }

  @Test
  public void testSetInvorted() {

    MatrixIJK m = new MatrixIJK();
    MatrixIJK toInvort = new MatrixIJK(50, ortho1, 12, ortho2, 5000, ortho3);

    MatrixIJK result = m.setToInvorted(toInvort);

    assertSame(result, m);
    assertEqualMatrix(new MatrixIJK(50, ortho1, 12, ortho2, 5000, ortho3), toInvort);
    assertComponentEquals(MatrixIJK.IDENTITY, MatrixIJK.mxm(result, toInvort), LOOSE_TOLERANCE);

  }

  /*
   * The suppress deprecation warning is issued for the following tests, which exercise deprecated
   * static methods on the MatrixIJK class. These should be left here until these methods are
   * removed from the class.
   */
  @SuppressWarnings("deprecation")
  @Test
  public void testMxvOverA() {
    VectorIJK d = MatrixIJK.mxv(m, a, a);
    assertSame(d, a);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualVector(new VectorIJK(56, 70, 92), a);
  }

  @SuppressWarnings("deprecation")
  @Test
  public void testMxv() {
    VectorIJK d = MatrixIJK.mxv(m, a, b);
    assertSame(d, b);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(56, 70, 92), b);
  }

  @SuppressWarnings("deprecation")
  @Test
  public void testNewMxv() {
    VectorIJK d = MatrixIJK.mxv(m, a);
    assertNotSame(d, a);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(56, 70, 92), d);
  }

  @SuppressWarnings("deprecation")
  @Test
  public void testMtxvOverA() {
    m.transpose();
    VectorIJK d = MatrixIJK.mtxv(m, a, a);
    assertSame(d, a);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9).transpose(), m);
    assertEqualVector(new VectorIJK(56, 70, 92), a);
  }

  @SuppressWarnings("deprecation")
  @Test
  public void testMtxv() {
    m.transpose();
    VectorIJK d = MatrixIJK.mtxv(m, a, b);
    assertSame(d, b);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9).transpose(), m);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(56, 70, 92), b);
  }

  @SuppressWarnings("deprecation")
  @Test
  public void testNewMtxv() {
    m.transpose();
    VectorIJK d = MatrixIJK.mtxv(m, a);
    assertNotSame(d, a);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9).transpose(), m);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(56, 70, 92), d);
  }

  @Test
  public void testAddUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJKOverM() {
    MatrixIJK r = MatrixIJK.add(m, n, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(new MatrixIJK(20, 25, 34, 21, 33, 47, 5.1, 7.2, 9.3), m);
  }

  @Test
  public void testAddUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJKOverN() {
    MatrixIJK r = MatrixIJK.add(m, n, n);
    assertSame(r, n);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(20, 25, 34, 21, 33, 47, 5.1, 7.2, 9.3), n);
  }

  @Test
  public void testAddUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJKMMOverM() {
    MatrixIJK r = MatrixIJK.add(m, m, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJK(38, 46, 62, 22, 26, 34, 10, 14, 18), m);
  }

  @Test
  public void testAddUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJK() {
    MatrixIJK r = MatrixIJK.add(m, n, o);
    assertSame(r, o);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(new MatrixIJK(20, 25, 34, 21, 33, 47, 5.1, 7.2, 9.3), o);
  }

  @Test
  public void testNewAddUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJK() {
    MatrixIJK r = MatrixIJK.add(m, n);
    assertNotSame(r, n);
    assertNotSame(r, m);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(new MatrixIJK(20, 25, 34, 21, 33, 47, 5.1, 7.2, 9.3), r);
  }

  @Test
  public void testSubtractUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJKOverM() {
    MatrixIJK r = MatrixIJK.subtract(m, n, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(new MatrixIJK(18, 21, 28, 1, -7, -13, 4.9, 6.8, 8.7), m);
  }

  @Test
  public void testSubtractUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJKOverN() {
    MatrixIJK r = MatrixIJK.subtract(m, n, n);
    assertSame(r, n);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(18, 21, 28, 1, -7, -13, 4.9, 6.8, 8.7), n);
  }

  @Test
  public void testSubtractUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJKMMOverM() {
    MatrixIJK r = MatrixIJK.subtract(m, m, m);
    assertSame(r, m);
    assertEqualMatrix(MatrixIJK.ZEROS, m);
  }

  @Test
  public void testSubtractUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJK() {
    MatrixIJK r = MatrixIJK.subtract(m, n, o);
    assertSame(r, o);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(new MatrixIJK(18, 21, 28, 1, -7, -13, 4.9, 6.8, 8.7), o);
  }

  @Test
  public void testNewSubtractUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJK() {
    MatrixIJK r = MatrixIJK.subtract(m, n);
    assertNotSame(r, m);
    assertNotSame(r, n);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(new MatrixIJK(18, 21, 28, 1, -7, -13, 4.9, 6.8, 8.7), r);
  }

  @Test
  public void testMxmUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJKOverM() {
    MatrixIJK r = MatrixIJK.mxm(m, n, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEquivalentMatrix(new MatrixIJK(56, 70, 92, 560, 700, 920, 5.6, 7.0, 9.2), r);
  }

  @Test
  public void testMxmUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJKOverN() {
    MatrixIJK r = MatrixIJK.mxm(m, n, n);
    assertSame(r, n);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEquivalentMatrix(new MatrixIJK(56, 70, 92, 560, 700, 920, 5.6, 7.0, 9.2), r);
  }

  @Test
  public void testMxmUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJKMMOverM() {
    MatrixIJK r = MatrixIJK.mxm(m, m, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJK(769, 953, 1259, 437, 541, 715, 217, 269, 355), m);
  }

  @Test
  public void testMxmUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJK() {
    MatrixIJK r = MatrixIJK.mxm(m, n, o);
    assertSame(r, o);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEquivalentMatrix(new MatrixIJK(56, 70, 92, 560, 700, 920, 5.6, 7.0, 9.2), r);
  }

  @Test
  public void testNewMxmUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJK() {
    MatrixIJK r = MatrixIJK.mxm(m, n);
    assertNotSame(r, n);
    assertNotSame(r, m);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEquivalentMatrix(new MatrixIJK(56, 70, 92, 560, 700, 920, 5.6, 7.0, 9.2), r);
  }

  @Test
  public void testMtxmUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJKOverM() {
    m.transpose();
    MatrixIJK r = MatrixIJK.mtxm(m, n, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEquivalentMatrix(new MatrixIJK(56, 70, 92, 560, 700, 920, 5.6, 7.0, 9.2), r);
  }

  @Test
  public void testMtxmUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJKOverN() {
    m.transpose();
    MatrixIJK r = MatrixIJK.mtxm(m, n, n);
    assertSame(r, n);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9).transpose(), m);
    assertEquivalentMatrix(new MatrixIJK(56, 70, 92, 560, 700, 920, 5.6, 7.0, 9.2), r);
  }

  @Test
  public void testMtxmUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJKMMOverM() {
    MatrixIJK r = MatrixIJK.mtxm(m, m, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJK(1851, 1035, 535, 1035, 579, 299, 535, 299, 155), m);
  }

  @Test
  public void testMtxmUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJK() {
    m.transpose();
    MatrixIJK r = MatrixIJK.mtxm(m, n, o);
    assertSame(r, o);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9).transpose(), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEquivalentMatrix(new MatrixIJK(56, 70, 92, 560, 700, 920, 5.6, 7.0, 9.2), r);
  }

  @Test
  public void testNewMtxmUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJK() {
    m.transpose();
    MatrixIJK r = MatrixIJK.mtxm(m, n);
    assertNotSame(r, n);
    assertNotSame(r, m);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9).transpose(), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEquivalentMatrix(new MatrixIJK(56, 70, 92, 560, 700, 920, 5.6, 7.0, 9.2), r);
  }

  @Test
  public void testMxmtUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJKOverM() {
    n.transpose();
    MatrixIJK r = MatrixIJK.mxmt(m, n, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3).transpose(), n);
    assertEquivalentMatrix(new MatrixIJK(56, 70, 92, 560, 700, 920, 5.6, 7.0, 9.2), r);
  }

  @Test
  public void testMxmtUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJKOverN() {
    n.transpose();
    MatrixIJK r = MatrixIJK.mxmt(m, n, n);
    assertSame(r, n);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEquivalentMatrix(new MatrixIJK(56, 70, 92, 560, 700, 920, 5.6, 7.0, 9.2), r);
  }

  @Test
  public void testMxmtUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJKMMOverM() {
    MatrixIJK r = MatrixIJK.mxmt(m, m, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJK(507, 615, 821, 615, 747, 997, 821, 997, 1331), m);
  }

  @Test
  public void testMxmtUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJK() {
    n.transpose();
    MatrixIJK r = MatrixIJK.mxmt(m, n, o);
    assertSame(r, o);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3).transpose(), n);
    assertEquivalentMatrix(new MatrixIJK(56, 70, 92, 560, 700, 920, 5.6, 7.0, 9.2), r);
  }

  @Test
  public void testNewMxmtUnwritableMatrixIJKUnwritableMatrixIJKMatrixIJK() {
    n.transpose();
    MatrixIJK r = MatrixIJK.mxmt(m, n);
    assertNotSame(r, n);
    assertNotSame(r, m);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3).transpose(), n);
    assertEquivalentMatrix(new MatrixIJK(56, 70, 92, 560, 700, 920, 5.6, 7.0, 9.2), r);
  }

  @Test
  public void testmxmaddOverK() {
    MatrixIJK tmp = MatrixIJK.mxm(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mxm(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mxmadd(k, l, m, n, k);
    assertSame(r, k);

    assertEqualMatrix(new MatrixIJK(1, 0, -1, 1, -1, 1, 2, -3, 4), l);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(expected, k);
  }

  @Test
  public void testmxmaddOverL() {
    MatrixIJK tmp = MatrixIJK.mxm(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mxm(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mxmadd(k, l, m, n, l);
    assertSame(r, l);

    assertEqualMatrix(new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34), k);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(expected, l);
  }

  @Test
  public void testmxmaddOverM() {
    MatrixIJK tmp = MatrixIJK.mxm(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mxm(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mxmadd(k, l, m, n, m);
    assertSame(r, m);

    assertEqualMatrix(new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34), k);
    assertEqualMatrix(new MatrixIJK(1, 0, -1, 1, -1, 1, 2, -3, 4), l);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testmxmaddOverN() {
    MatrixIJK tmp = MatrixIJK.mxm(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mxm(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mxmadd(k, l, m, n, n);
    assertSame(r, n);

    assertEqualMatrix(new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34), k);
    assertEqualMatrix(new MatrixIJK(1, 0, -1, 1, -1, 1, 2, -3, 4), l);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(expected, n);
  }

  @Test
  public void testmxmaddMMMMOverM() {
    MatrixIJK tmp = MatrixIJK.mxm(m, m, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.add(tmp, tmp, new MatrixIJK());

    MatrixIJK r = MatrixIJK.mxmadd(m, m, m, m, m);
    assertSame(r, m);

    assertEqualMatrix(expected, m);
  }

  @Test
  public void testmxmadd() {
    MatrixIJK tmp = MatrixIJK.mxm(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mxm(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mxmadd(k, l, m, n, o);
    assertSame(r, o);

    assertEqualMatrix(new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34), k);
    assertEqualMatrix(new MatrixIJK(1, 0, -1, 1, -1, 1, 2, -3, 4), l);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(expected, o);
  }

  @Test
  public void testNewmxmadd() {
    MatrixIJK tmp = MatrixIJK.mxm(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mxm(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mxmadd(k, l, m, n);
    assertNotSame(r, k);
    assertNotSame(r, l);
    assertNotSame(r, m);
    assertNotSame(r, n);

    assertEqualMatrix(new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34), k);
    assertEqualMatrix(new MatrixIJK(1, 0, -1, 1, -1, 1, 2, -3, 4), l);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(expected, r);
  }

  @Test
  public void testmtxmaddOverK() {
    MatrixIJK tmp = MatrixIJK.mtxm(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mtxm(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mtxmadd(k, l, m, n, k);
    assertSame(r, k);

    assertEqualMatrix(new MatrixIJK(1, 0, -1, 1, -1, 1, 2, -3, 4), l);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(expected, k);
  }

  @Test
  public void testmtxmaddOverL() {
    MatrixIJK tmp = MatrixIJK.mtxm(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mtxm(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mtxmadd(k, l, m, n, l);
    assertSame(r, l);

    assertEqualMatrix(new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34), k);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(expected, l);
  }

  @Test
  public void testmtxmaddOverM() {
    MatrixIJK tmp = MatrixIJK.mtxm(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mtxm(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mtxmadd(k, l, m, n, m);
    assertSame(r, m);

    assertEqualMatrix(new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34), k);
    assertEqualMatrix(new MatrixIJK(1, 0, -1, 1, -1, 1, 2, -3, 4), l);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testmtxmaddOverN() {
    MatrixIJK tmp = MatrixIJK.mtxm(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mtxm(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mtxmadd(k, l, m, n, n);
    assertSame(r, n);

    assertEqualMatrix(new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34), k);
    assertEqualMatrix(new MatrixIJK(1, 0, -1, 1, -1, 1, 2, -3, 4), l);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(expected, n);
  }

  @Test
  public void testmtxmaddMMMMOverM() {
    MatrixIJK tmp = MatrixIJK.mtxm(m, m, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.add(tmp, tmp, new MatrixIJK());

    MatrixIJK r = MatrixIJK.mtxmadd(m, m, m, m, m);
    assertSame(r, m);

    assertEqualMatrix(expected, m);
  }

  @Test
  public void testmtxmadd() {
    MatrixIJK tmp = MatrixIJK.mtxm(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mtxm(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mtxmadd(k, l, m, n, o);
    assertSame(r, o);

    assertEqualMatrix(new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34), k);
    assertEqualMatrix(new MatrixIJK(1, 0, -1, 1, -1, 1, 2, -3, 4), l);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(expected, o);
  }

  @Test
  public void testNewmtxmadd() {
    MatrixIJK tmp = MatrixIJK.mtxm(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mtxm(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mtxmadd(k, l, m, n);
    assertNotSame(r, k);
    assertNotSame(r, l);
    assertNotSame(r, m);
    assertNotSame(r, n);

    assertEqualMatrix(new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34), k);
    assertEqualMatrix(new MatrixIJK(1, 0, -1, 1, -1, 1, 2, -3, 4), l);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(expected, r);
  }

  @Test
  public void testmxmtaddOverK() {
    MatrixIJK tmp = MatrixIJK.mxmt(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mxmt(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mxmtadd(k, l, m, n, k);
    assertSame(r, k);

    assertEqualMatrix(new MatrixIJK(1, 0, -1, 1, -1, 1, 2, -3, 4), l);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(expected, k);
  }

  @Test
  public void testmxmtaddOverL() {
    MatrixIJK tmp = MatrixIJK.mxmt(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mxmt(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mxmtadd(k, l, m, n, l);
    assertSame(r, l);

    assertEqualMatrix(new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34), k);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(expected, l);
  }

  @Test
  public void testmxmtaddOverM() {
    MatrixIJK tmp = MatrixIJK.mxmt(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mxmt(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mxmtadd(k, l, m, n, m);
    assertSame(r, m);

    assertEqualMatrix(new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34), k);
    assertEqualMatrix(new MatrixIJK(1, 0, -1, 1, -1, 1, 2, -3, 4), l);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testmxmtaddOverN() {
    MatrixIJK tmp = MatrixIJK.mxmt(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mxmt(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mxmtadd(k, l, m, n, n);
    assertSame(r, n);

    assertEqualMatrix(new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34), k);
    assertEqualMatrix(new MatrixIJK(1, 0, -1, 1, -1, 1, 2, -3, 4), l);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(expected, n);
  }

  @Test
  public void testmxmtaddMMMMOverM() {
    MatrixIJK tmp = MatrixIJK.mxmt(m, m, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.add(tmp, tmp, new MatrixIJK());

    MatrixIJK r = MatrixIJK.mxmtadd(m, m, m, m, m);
    assertSame(r, m);

    assertEqualMatrix(expected, m);
  }

  @Test
  public void testmxmtadd() {
    MatrixIJK tmp = MatrixIJK.mxmt(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mxmt(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mxmtadd(k, l, m, n, o);
    assertSame(r, o);

    assertEqualMatrix(new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34), k);
    assertEqualMatrix(new MatrixIJK(1, 0, -1, 1, -1, 1, 2, -3, 4), l);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(expected, o);
  }

  @Test
  public void testNewmxmtadd() {
    MatrixIJK tmp = MatrixIJK.mxmt(k, l, new MatrixIJK());
    MatrixIJK expected = MatrixIJK.mxmt(m, n, new MatrixIJK());
    MatrixIJK.add(tmp, expected, expected);

    MatrixIJK r = MatrixIJK.mxmtadd(k, l, m, n);
    assertNotSame(r, k);
    assertNotSame(r, l);
    assertNotSame(r, m);
    assertNotSame(r, n);

    assertEqualMatrix(new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34), k);
    assertEqualMatrix(new MatrixIJK(1, 0, -1, 1, -1, 1, 2, -3, 4), l);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualMatrix(new MatrixIJK(1, 2, 3, 10, 20, 30, 0.1, 0.2, 0.3), n);
    assertEqualMatrix(expected, r);
  }

  @Test
  public void testCreateUnitizedColumns() {
    MatrixIJK unitizeColumns = valueCon.createUnitizedColumns();
    assertNotSame(unitizeColumns, valueCon);

    double lc1 = Math.sqrt(14.0);
    double lc2 = Math.sqrt(77.0);
    double lc3 = Math.sqrt(194.0);

    assertComponentEquals(new UnwritableMatrixIJK(-1.0 / lc1, 2.0 / lc1, -3.0 / lc1, 4.0 / lc2,
        -5 / lc2, 6 / lc2, -7.0 / lc3, 8.0 / lc3, -9.0 / lc3), unitizeColumns, LOOSE_TOLERANCE);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, 4, -5, 6, -7, 8, -9), valueCon);
  }

  @Test
  public void testUnitizeColumns() {
    MatrixIJK unitizeColumns = valueCon.unitizeColumns();
    assertSame(unitizeColumns, valueCon);

    double lc1 = Math.sqrt(14.0);
    double lc2 = Math.sqrt(77.0);
    double lc3 = Math.sqrt(194.0);

    assertComponentEquals(new UnwritableMatrixIJK(-1.0 / lc1, 2.0 / lc1, -3.0 / lc1, 4.0 / lc2,
        -5 / lc2, 6 / lc2, -7.0 / lc3, 8.0 / lc3, -9.0 / lc3), unitizeColumns, LOOSE_TOLERANCE);
  }

  @Test
  public void testSetToUnitizedColumns() {
    MatrixIJK unitizeColumns = arrayCon.setToUnitizedColumns(valueCon);
    assertSame(unitizeColumns, arrayCon);

    double lc1 = Math.sqrt(14.0);
    double lc2 = Math.sqrt(77.0);
    double lc3 = Math.sqrt(194.0);

    assertComponentEquals(new UnwritableMatrixIJK(-1.0 / lc1, 2.0 / lc1, -3.0 / lc1, 4.0 / lc2,
        -5 / lc2, 6 / lc2, -7.0 / lc3, 8.0 / lc3, -9.0 / lc3), unitizeColumns, LOOSE_TOLERANCE);
    assertEqualMatrix(new UnwritableMatrixIJK(-1, 2, -3, 4, -5, 6, -7, 8, -9), valueCon);
  }

  static void checkStaticFinalMembers() {

    assertEqualMatrix(new UnwritableMatrixIJK(1, 0, 0, 0, 1, 0, 0, 0, 1), MatrixIJK.IDENTITY);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 1, 1, 1, 1, 1, 1, 1, 1), MatrixIJK.ONES);
    assertEqualMatrix(new UnwritableMatrixIJK(0, 0, 0, 0, 0, 0, 0, 0, 0), MatrixIJK.ZEROS);

  }

}
