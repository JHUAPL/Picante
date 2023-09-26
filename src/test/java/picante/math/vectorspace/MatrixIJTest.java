package picante.math.vectorspace;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEqualMatrix;
import static picante.junit.AssertTools.assertEqualVector;
import static picante.junit.AssertTools.assertEquivalentMatrix;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import picante.junit.AssertTools;

/*
 * TODO: Modify inversion test routines that take a threshold to check threshold is implemented
 * properly.
 */
public class MatrixIJTest {

  private static final double LOOSE_TOLERANCE = 1.0E-14;

  private MatrixIJ defaultCon;
  private MatrixIJ valueCon;
  private MatrixIJ arrayCon;
  private MatrixIJ copyCon;
  private MatrixIJ scaleCon;
  private MatrixIJ vectorCon;
  private MatrixIJ scaleVectorCon;
  private MatrixIJ zeroDetCon;

  private double[][] array;
  private UnwritableVectorIJ ithCol;
  private UnwritableVectorIJ jthCol;
  private UnwritableVectorIJ ortho1;
  private UnwritableVectorIJ ortho2;

  private MatrixIJ k;
  private MatrixIJ l;
  private MatrixIJ m;
  private MatrixIJ n;
  private MatrixIJ o;

  @Before
  public void setUp() throws Exception {
    array = new double[][] {{1, -2}, {3, -4}};
    ithCol = new UnwritableVectorIJ(1, -2);
    jthCol = new UnwritableVectorIJ(-5, 7);

    defaultCon = new MatrixIJ();
    valueCon = new MatrixIJ(-1, 2, -3, 4);
    arrayCon = new MatrixIJ(array);
    copyCon = new MatrixIJ(valueCon);
    scaleCon = new MatrixIJ(-2.0, valueCon);
    vectorCon = new MatrixIJ(ithCol, jthCol);
    scaleVectorCon = new MatrixIJ(3.0, ithCol, -2.0, jthCol);
    zeroDetCon = new MatrixIJ(1.5, 2, 3, 4);

    ortho1 = new UnwritableVectorIJ(1, 2).createUnitized();
    ortho2 = new UnwritableVectorIJ(-2, 1).createUnitized();

    k = new MatrixIJ(1, 1, 2, 3);
    l = new MatrixIJ(1, 0, -1, 1);
    m = new MatrixIJ(19, 23, 31, 11);
    n = new MatrixIJ(1, 2, 3, 10);
    o = new MatrixIJ();

  }

  @After
  public void tearDown() throws Exception {
    checkStaticFinalMembers();
  }

  @Test
  public void testMatrixIJ() {
    assertEqualMatrix(new UnwritableMatrixIJ(1, 0, 0, 1), defaultCon);
  }

  @Test
  public void testMatrixIJDoubleDoubleDoubleDouble() {
    assertEqualMatrix(new UnwritableMatrixIJ(-1, 2, -3, 4), valueCon);
  }

  @Test
  public void testMatrixIJDoubleUnwritableMatrixIJ() {
    assertEqualMatrix(new UnwritableMatrixIJ(2, -4, 6, -8), scaleCon);
    assertEqualMatrix(new UnwritableMatrixIJ(-1, 2, -3, 4), valueCon);
  }

  @Test
  public void testMatrixIJDoubleUnwritableVectorIJDoubleUnwritableVectorIJ() {
    assertEqualMatrix(new UnwritableMatrixIJ(3, -6, 10, -14), scaleVectorCon);
    assertEqualVector(new UnwritableVectorIJ(1, -2), ithCol);
    assertEqualVector(new UnwritableVectorIJ(-5, 7), jthCol);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testMatrixIJDoubleArrayArrayException() {
    double[][] badArray = {{1}, {3}};
    new MatrixIJ(badArray);
  }

  @Test
  public void testMatrixIJDoubleArrayArray() {
    assertEqualMatrix(new UnwritableMatrixIJ(1, 3, -2, -4), arrayCon);
    AssertTools.assertEqualDouble(1, array[0][0]);
    AssertTools.assertEqualDouble(-2, array[0][1]);
    AssertTools.assertEqualDouble(3, array[1][0]);
    AssertTools.assertEqualDouble(-4, array[1][1]);

  }

  @Test
  public void testMatrixIJUnwritableMatrixIJ() {
    assertEqualMatrix(new UnwritableMatrixIJ(-1, 2, -3, 4), copyCon);
    assertEqualMatrix(new UnwritableMatrixIJ(-1, 2, -3, 4), valueCon);
  }

  @Test
  public void testMatrixIJUnwritableVectorIJUnwritableVectorIJ() {
    assertEqualMatrix(new UnwritableMatrixIJ(1, -2, -5, 7), vectorCon);

    assertEqualVector(new UnwritableVectorIJ(1, -2), ithCol);
    assertEqualVector(new UnwritableVectorIJ(-5, 7), jthCol);
  }

  @Test
  public void testCreateTranspose() {
    MatrixIJ transpose = valueCon.createTranspose();
    assertNotSame(transpose, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(-1, -3, 2, 4), transpose);
    assertEqualMatrix(new UnwritableMatrixIJ(-1, 2, -3, 4), valueCon);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInverseException() {
    zeroDetCon.createInverse();
  }

  @Test
  public void testCreateInverse() {
    MatrixIJ inverse = vectorCon.createInverse();
    assertNotSame(inverse, vectorCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, -2, -5, 7), vectorCon);
    UnwritableMatrixIJ product = MatrixIJ.mxm(inverse, vectorCon, new MatrixIJ());
    assertComponentEquals(MatrixIJ.IDENTITY, product, LOOSE_TOLERANCE);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInverseDoubleException() {
    zeroDetCon.createInverse(0.0001);
  }

  @Test
  public void testCreateInverseDouble() {
    MatrixIJ inverse = vectorCon.createInverse(0.0001);
    assertNotSame(inverse, vectorCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, -2, -5, 7), vectorCon);
    MatrixIJ product = MatrixIJ.mxm(inverse, vectorCon, new MatrixIJ());
    assertComponentEquals(MatrixIJ.IDENTITY, product, LOOSE_TOLERANCE);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInvortedIthColumnException() {
    new MatrixIJ(1E-312, ortho2, 10, ortho1).createInvorted();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInvortedJthColumnException() {
    new MatrixIJ(1E30, ortho2, 1E-312, ortho1).createInvorted();
  }

  @Test
  public void testCreateInvorted() {
    MatrixIJ m = new MatrixIJ(5, ortho1, 3.1E30, ortho2);
    MatrixIJ inverseM = m.createInvorted();
    assertNotSame(m, inverseM);
    assertEqualMatrix(new MatrixIJ(5, ortho1, 3.1E30, ortho2), m);
    assertComponentEquals(MatrixIJ.IDENTITY, MatrixIJ.mxm(m, inverseM), LOOSE_TOLERANCE);
  }

  @Test
  public void testTranspose() {
    MatrixIJ transpose = valueCon.transpose();
    assertSame(transpose, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(-1, -3, 2, 4), valueCon);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testInvertException() {
    zeroDetCon.invert();
  }

  @Test
  public void testInvert() {
    MatrixIJ copy = new MatrixIJ(vectorCon);
    MatrixIJ inverse = vectorCon.invert();
    assertSame(inverse, vectorCon);
    MatrixIJ product = MatrixIJ.mxm(inverse, copy, new MatrixIJ());
    assertComponentEquals(MatrixIJ.IDENTITY, product, LOOSE_TOLERANCE);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testInvertDoubleException() {
    zeroDetCon.invert(0.0001);
  }

  @Test
  public void testInvertDouble() {
    MatrixIJ copy = new MatrixIJ(vectorCon);
    MatrixIJ inverse = vectorCon.invert(0.00001);
    assertSame(inverse, vectorCon);
    MatrixIJ product = MatrixIJ.mxm(inverse, copy, new MatrixIJ());
    assertComponentEquals(MatrixIJ.IDENTITY, product, LOOSE_TOLERANCE);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testInvortIthColumnException() {
    new MatrixIJ(1E-312, ortho2, 10, ortho1).invort();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testInvortJthColumnException() {
    new MatrixIJ(1E30, ortho2, 1E-312, ortho1).invort();

  }

  @Test
  public void testInvort() {

    MatrixIJ toInvort = new MatrixIJ(50, ortho1, 12, ortho2);

    MatrixIJ result = toInvort.invort();

    assertSame(result, toInvort);
    assertComponentEquals(MatrixIJ.IDENTITY,
        MatrixIJ.mxm(result, new MatrixIJ(50, ortho1, 12, ortho2)), LOOSE_TOLERANCE);
  }

  @Test
  public void testScaleDouble() {
    MatrixIJ scale = valueCon.scale(2.0);
    assertSame(scale, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(-2, 4, -6, 8), valueCon);
  }

  @Test
  public void testScaleDoubleDouble() {
    MatrixIJ scale = valueCon.scale(-1, 2);
    assertSame(scale, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, -2, -6, 8), valueCon);
  }

  @Test
  public void testSetII() {
    valueCon.setII(Math.PI);
    assertEqualMatrix(new UnwritableMatrixIJ(Math.PI, 2, -3, 4), valueCon);
  }

  @Test
  public void testSetJI() {
    valueCon.setJI(Math.PI);
    assertEqualMatrix(new UnwritableMatrixIJ(-1, Math.PI, -3, 4), valueCon);
  }

  @Test
  public void testSetIJ() {
    valueCon.setIJ(Math.PI);
    assertEqualMatrix(new UnwritableMatrixIJ(-1, 2, Math.PI, 4), valueCon);
  }

  @Test
  public void testSetJJ() {
    valueCon.setJJ(Math.PI);
    assertEqualMatrix(new UnwritableMatrixIJ(-1, 2, -3, Math.PI), valueCon);
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
    assertEqualMatrix(new UnwritableMatrixIJ(50, 2, -3, 4), valueCon);

    valueCon.set(1, 0, 51);
    assertEqualMatrix(new UnwritableMatrixIJ(50, 51, -3, 4), valueCon);

    valueCon.set(0, 1, 52);
    assertEqualMatrix(new UnwritableMatrixIJ(50, 51, 52, 4), valueCon);

    valueCon.set(1, 1, 53);
    assertEqualMatrix(new UnwritableMatrixIJ(50, 51, 52, 53), valueCon);

  }

  @Test
  public void testSetIthColumn() {
    UnwritableVectorIJ column = new UnwritableVectorIJ(10, 11);
    valueCon.setIthColumn(column);
    assertEqualMatrix(new UnwritableMatrixIJ(10, 11, -3, 4), valueCon);
    assertEqualVector(new UnwritableVectorIJ(10, 11), column);
  }

  @Test
  public void testSetJthColumn() {
    UnwritableVectorIJ column = new UnwritableVectorIJ(10, 11);
    valueCon.setJthColumn(column);
    assertEqualMatrix(new UnwritableMatrixIJ(-1, 2, 10, 11), valueCon);
    assertEqualVector(new UnwritableVectorIJ(10, 11), column);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSetColumnException() {
    valueCon.setColumn(-1, new VectorIJ());
  }

  @Test
  public void testSetColumn() {
    UnwritableVectorIJ column = new UnwritableVectorIJ(10, 11);
    valueCon.setColumn(0, column);
    assertEqualMatrix(new UnwritableMatrixIJ(10, 11, -3, 4), valueCon);
    assertEqualVector(new UnwritableVectorIJ(10, 11), column);

    column = new UnwritableVectorIJ(13, 14);
    valueCon.setColumn(1, column);
    assertEqualMatrix(new UnwritableMatrixIJ(10, 11, 13, 14), valueCon);
    assertEqualVector(new UnwritableVectorIJ(13, 14), column);

  }

  @Test
  public void testSetToDoubleDoubleDoubleDouble() {
    MatrixIJ result = valueCon.setTo(50, 51, 52, 53);
    assertSame(result, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(50, 51, 52, 53), valueCon);
  }

  @Test
  public void testSetToUnwritableVectorIJUnwritableVectorIJ() {
    UnwritableVectorIJ i = new UnwritableVectorIJ(50, 51);
    UnwritableVectorIJ j = new UnwritableVectorIJ(53, 54);
    MatrixIJ result = valueCon.setTo(i, j);
    assertSame(result, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(50, 51, 53, 54), valueCon);
    assertEqualVector(new UnwritableVectorIJ(50, 51), i);
    assertEqualVector(new UnwritableVectorIJ(53, 54), j);
  }

  @Test
  public void testSetToDoubleUnwritableVectorIJDoubleUnwritableVectorIJ() {
    UnwritableVectorIJ i = new UnwritableVectorIJ(1, 2);
    UnwritableVectorIJ j = new UnwritableVectorIJ(5, 7);
    MatrixIJ result = valueCon.setTo(2, i, -3, j);
    assertSame(result, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(2, 4, -15, -21), valueCon);
    assertEqualVector(new UnwritableVectorIJ(1, 2), i);
    assertEqualVector(new UnwritableVectorIJ(5, 7), j);
  }

  @Test
  public void testSetToUnwritableMatrixIJ() {
    MatrixIJ matrix = valueCon.setTo(arrayCon);
    assertSame(matrix, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 3, -2, -4), valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 3, -2, -4), arrayCon);
  }

  @Test
  public void testSetToDoubleUnwritableMatrixIJ() {
    MatrixIJ result = valueCon.setTo(2.0, vectorCon);
    assertSame(result, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(2, -4, -10, 14), valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, -2, -5, 7), vectorCon);
  }

  @Test
  public void testSetToDoubleDoubleUnwritableMatrixIJ() {
    MatrixIJ result = valueCon.setTo(2.0, -3.0, arrayCon);
    assertSame(result, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(2, 6, 6, 12), valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 3, -2, -4), arrayCon);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testSetToDoubleArrayArrayException() {
    double[][] badArray = {{1}, {3}};
    valueCon.setTo(badArray);
  }

  @Test
  public void testSetToDoubleArrayArray() {
    assertEqualMatrix(new UnwritableMatrixIJ(1, 3, -2, -4), arrayCon);
    AssertTools.assertEqualDouble(1, array[0][0]);
    AssertTools.assertEqualDouble(-2, array[0][1]);
    AssertTools.assertEqualDouble(3, array[1][0]);
    AssertTools.assertEqualDouble(-4, array[1][1]);

  }

  @Test
  public void testSetToTranspose() {
    MatrixIJ transpose = valueCon.setToTranspose(arrayCon);
    assertSame(transpose, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, -2, 3, -4), valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 3, -2, -4), arrayCon);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSetToInverseUnwritableMatrixIJException() {
    vectorCon.setToInverse(zeroDetCon);
  }

  @Test
  public void testSetToInverseUnwritableMatrixIJExceptionNoChange() {
    try {
      vectorCon.setToInverse(zeroDetCon);
      fail("Expected exception was not thrown.");
    } catch (IllegalArgumentException e) {
      assertEqualMatrix(new UnwritableMatrixIJ(1, -2, -5, 7), vectorCon);
      assertEqualMatrix(new UnwritableMatrixIJ(-1, 2, -3, 4), valueCon);
    }
  }

  @Test
  public void testSetToInverseUnwritableMatrixIJ() {
    MatrixIJ inverse = valueCon.setToInverse(vectorCon);
    assertSame(inverse, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, -2, -5, 7), vectorCon);
    MatrixIJ product = MatrixIJ.mxm(inverse, vectorCon, new MatrixIJ());
    assertComponentEquals(MatrixIJ.IDENTITY, product, LOOSE_TOLERANCE);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSetToInverseUnwritableMatrixIJDoubleException() {
    vectorCon.setToInverse(zeroDetCon, 0.0001);
  }

  @Test
  public void testSetToInverseUnwritableMatrixIJDoubleExceptionNoChange() {
    try {
      vectorCon.setToInverse(zeroDetCon, 0.0001);
      fail("Expected exception was not thrown.");
    } catch (IllegalArgumentException e) {
      assertEqualMatrix(new UnwritableMatrixIJ(1, -2, -5, 7), vectorCon);
      assertEqualMatrix(new UnwritableMatrixIJ(-1, 2, -3, 4), valueCon);
    }
  }

  @Test
  public void testSetToInverseUnwritableMatrixIJDouble() {
    MatrixIJ inverse = valueCon.setToInverse(vectorCon, 0.0001);
    assertSame(inverse, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, -2, -5, 7), vectorCon);
    MatrixIJ product = MatrixIJ.mxm(inverse, vectorCon, new MatrixIJ());
    assertComponentEquals(MatrixIJ.IDENTITY, product, LOOSE_TOLERANCE);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testSetInvortedIthColumnException() {
    new MatrixIJ().setToInvorted(new MatrixIJ(1E-312, ortho2, 10, ortho1));
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testSetInvortedJthColumnException() {
    new MatrixIJ().setToInvorted(new MatrixIJ(1E30, ortho2, 1E-312, ortho1));
  }

  @Test
  public void testSetInvorted() {

    MatrixIJ m = new MatrixIJ();
    MatrixIJ toInvort = new MatrixIJ(50, ortho1, 12, ortho2);

    MatrixIJ result = m.setToInvorted(toInvort);

    assertSame(result, m);
    assertEqualMatrix(new MatrixIJ(50, ortho1, 12, ortho2), toInvort);
    assertComponentEquals(MatrixIJ.IDENTITY, MatrixIJ.mxm(result, toInvort), LOOSE_TOLERANCE);
  }

  @Test
  public void testAddUnwritableMatrixIJUnwritableMatrixIJMatrixIJOverM() {
    MatrixIJ r = MatrixIJ.add(m, n, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(new MatrixIJ(20, 25, 34, 21), m);
  }

  @Test
  public void testAddUnwritableMatrixIJUnwritableMatrixIJMatrixIJOverN() {
    MatrixIJ r = MatrixIJ.add(m, n, n);
    assertSame(r, n);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(20, 25, 34, 21), n);
  }

  @Test
  public void testAddUnwritableMatrixIJUnwritableMatrixIJMatrixIJMMOverM() {
    MatrixIJ r = MatrixIJ.add(m, m, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJ(38, 46, 62, 22), m);
  }

  @Test
  public void testAddUnwritableMatrixIJUnwritableMatrixIJMatrixIJ() {
    MatrixIJ r = MatrixIJ.add(m, n, o);
    assertSame(r, o);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(new MatrixIJ(20, 25, 34, 21), o);
  }

  @Test
  public void testNewAddUnwritableMatrixIJUnwritableMatrixIJMatrixIJ() {
    MatrixIJ r = MatrixIJ.add(m, n);
    assertNotSame(r, n);
    assertNotSame(r, m);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(new MatrixIJ(20, 25, 34, 21), r);
  }

  @Test
  public void testSubtractUnwritableMatrixIJUnwritableMatrixIJMatrixIJOverM() {
    MatrixIJ r = MatrixIJ.subtract(m, n, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(new MatrixIJ(18, 21, 28, 1), m);
  }

  @Test
  public void testSubtractUnwritableMatrixIJUnwritableMatrixIJMatrixIJOverN() {
    MatrixIJ r = MatrixIJ.subtract(m, n, n);
    assertSame(r, n);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(18, 21, 28, 1), n);
  }

  @Test
  public void testSubtractUnwritableMatrixIJUnwritableMatrixIJMatrixIJMMOverM() {
    MatrixIJ r = MatrixIJ.subtract(m, m, m);
    assertSame(r, m);
    assertEqualMatrix(MatrixIJ.ZEROS, m);
  }

  @Test
  public void testSubtractUnwritableMatrixIJUnwritableMatrixIJMatrixIJ() {
    MatrixIJ r = MatrixIJ.subtract(m, n, o);
    assertSame(r, o);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(new MatrixIJ(18, 21, 28, 1), o);
  }

  @Test
  public void testNewSubtractUnwritableMatrixIJUnwritableMatrixIJMatrixIJ() {
    MatrixIJ r = MatrixIJ.subtract(m, n);
    assertNotSame(r, m);
    assertNotSame(r, n);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(new MatrixIJ(18, 21, 28, 1), r);
  }

  @Test
  public void testMxmUnwritableMatrixIJUnwritableMatrixIJMatrixIJOverM() {
    MatrixIJ r = MatrixIJ.mxm(m, n, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEquivalentMatrix(new MatrixIJ(81, 45, 367, 179), r);
  }

  @Test
  public void testMxmUnwritableMatrixIJUnwritableMatrixIJMatrixIJOverN() {
    MatrixIJ r = MatrixIJ.mxm(m, n, n);
    assertSame(r, n);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEquivalentMatrix(new MatrixIJ(81, 45, 367, 179), r);
  }

  @Test
  public void testMxmUnwritableMatrixIJUnwritableMatrixIJMatrixIJMMOverM() {
    MatrixIJ r = MatrixIJ.mxm(m, m, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJ(179 * 6, 115 * 6, 155 * 6, 139 * 6), m);
  }

  @Test
  public void testMxmUnwritableMatrixIJUnwritableMatrixIJMatrixIJ() {
    MatrixIJ r = MatrixIJ.mxm(m, n, o);
    assertSame(r, o);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEquivalentMatrix(new MatrixIJ(81, 45, 367, 179), r);
  }

  @Test
  public void testNewMxmUnwritableMatrixIJUnwritableMatrixIJMatrixIJ() {
    MatrixIJ r = MatrixIJ.mxm(m, n);
    assertNotSame(r, n);
    assertNotSame(r, m);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEquivalentMatrix(new MatrixIJ(81, 45, 367, 179), r);
  }

  @Test
  public void testMtxmUnwritableMatrixIJUnwritableMatrixIJMatrixIJOverM() {
    m.transpose();
    MatrixIJ r = MatrixIJ.mtxm(m, n, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEquivalentMatrix(new MatrixIJ(81, 45, 367, 179), r);
  }

  @Test
  public void testMtxmUnwritableMatrixIJUnwritableMatrixIJMatrixIJOverN() {
    m.transpose();
    MatrixIJ r = MatrixIJ.mtxm(m, n, n);
    assertSame(r, n);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11).transpose(), m);
    assertEquivalentMatrix(new MatrixIJ(81, 45, 367, 179), r);
  }

  @Test
  public void testMtxmUnwritableMatrixIJUnwritableMatrixIJMatrixIJMMOverM() {
    MatrixIJ r = MatrixIJ.mtxm(m, m, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJ(2 * 445, 2 * 421, 2 * 421, 2 * 541), m);
  }

  @Test
  public void testMtxmUnwritableMatrixIJUnwritableMatrixIJMatrixIJ() {
    m.transpose();
    MatrixIJ r = MatrixIJ.mtxm(m, n, o);
    assertSame(r, o);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11).transpose(), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEquivalentMatrix(new MatrixIJ(81, 45, 367, 179), r);
  }

  @Test
  public void testNewMtxmUnwritableMatrixIJUnwritableMatrixIJMatrixIJ() {
    m.transpose();
    MatrixIJ r = MatrixIJ.mtxm(m, n);
    assertNotSame(r, n);
    assertNotSame(r, m);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11).transpose(), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEquivalentMatrix(new MatrixIJ(81, 45, 367, 179), r);
  }

  @Test
  public void testMxmtUnwritableMatrixIJUnwritableMatrixIJMatrixIJOverM() {
    n.transpose();
    MatrixIJ r = MatrixIJ.mxmt(m, n, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10).transpose(), n);
    assertEquivalentMatrix(new MatrixIJ(81, 45, 367, 179), r);
  }

  @Test
  public void testMxmtUnwritableMatrixIJUnwritableMatrixIJMatrixIJOverN() {
    n.transpose();
    MatrixIJ r = MatrixIJ.mxmt(m, n, n);
    assertSame(r, n);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEquivalentMatrix(new MatrixIJ(81, 45, 367, 179), r);
  }

  @Test
  public void testMxmtUnwritableMatrixIJUnwritableMatrixIJMatrixIJMMOverM() {
    MatrixIJ r = MatrixIJ.mxmt(m, m, m);
    assertSame(r, m);
    assertEqualMatrix(new MatrixIJ(2 * 661, 2 * 389, 2 * 389, 2 * 325), m);
  }

  @Test
  public void testMxmtUnwritableMatrixIJUnwritableMatrixIJMatrixIJ() {
    n.transpose();
    MatrixIJ r = MatrixIJ.mxmt(m, n, o);
    assertSame(r, o);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10).transpose(), n);
    assertEquivalentMatrix(new MatrixIJ(81, 45, 367, 179), r);
  }

  @Test
  public void testNewMxmtUnwritableMatrixIJUnwritableMatrixIJMatrixIJ() {
    n.transpose();
    MatrixIJ r = MatrixIJ.mxmt(m, n);
    assertNotSame(r, n);
    assertNotSame(r, m);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10).transpose(), n);
    assertEquivalentMatrix(new MatrixIJ(81, 45, 367, 179), r);
  }

  @Test
  public void testmxmaddOverK() {
    MatrixIJ tmp = MatrixIJ.mxm(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mxm(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mxmadd(k, l, m, n, k);
    assertSame(r, k);

    assertEqualMatrix(new MatrixIJ(1, 0, -1, 1), l);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(expected, k);
  }

  @Test
  public void testmxmaddOverL() {
    MatrixIJ tmp = MatrixIJ.mxm(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mxm(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mxmadd(k, l, m, n, l);
    assertSame(r, l);

    assertEqualMatrix(new MatrixIJ(1, 1, 2, 3), k);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(expected, l);
  }

  @Test
  public void testmxmaddOverM() {
    MatrixIJ tmp = MatrixIJ.mxm(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mxm(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mxmadd(k, l, m, n, m);
    assertSame(r, m);

    assertEqualMatrix(new MatrixIJ(1, 1, 2, 3), k);
    assertEqualMatrix(new MatrixIJ(1, 0, -1, 1), l);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testmxmaddOverN() {
    MatrixIJ tmp = MatrixIJ.mxm(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mxm(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mxmadd(k, l, m, n, n);
    assertSame(r, n);

    assertEqualMatrix(new MatrixIJ(1, 1, 2, 3), k);
    assertEqualMatrix(new MatrixIJ(1, 0, -1, 1), l);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(expected, n);
  }

  @Test
  public void testmxmaddMMMMOverM() {
    MatrixIJ tmp = MatrixIJ.mxm(m, m, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.add(tmp, tmp, new MatrixIJ());

    MatrixIJ r = MatrixIJ.mxmadd(m, m, m, m, m);
    assertSame(r, m);

    assertEqualMatrix(expected, m);
  }

  @Test
  public void testmxmadd() {
    MatrixIJ tmp = MatrixIJ.mxm(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mxm(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mxmadd(k, l, m, n, o);
    assertSame(r, o);

    assertEqualMatrix(new MatrixIJ(1, 1, 2, 3), k);
    assertEqualMatrix(new MatrixIJ(1, 0, -1, 1), l);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(expected, o);
  }

  @Test
  public void testNewmxmadd() {
    MatrixIJ tmp = MatrixIJ.mxm(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mxm(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mxmadd(k, l, m, n);
    assertNotSame(r, k);
    assertNotSame(r, l);
    assertNotSame(r, m);
    assertNotSame(r, n);

    assertEqualMatrix(new MatrixIJ(1, 1, 2, 3), k);
    assertEqualMatrix(new MatrixIJ(1, 0, -1, 1), l);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(expected, r);
  }

  @Test
  public void testmtxmaddOverK() {
    MatrixIJ tmp = MatrixIJ.mtxm(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mtxm(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mtxmadd(k, l, m, n, k);
    assertSame(r, k);

    assertEqualMatrix(new MatrixIJ(1, 0, -1, 1), l);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(expected, k);
  }

  @Test
  public void testmtxmaddOverL() {
    MatrixIJ tmp = MatrixIJ.mtxm(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mtxm(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mtxmadd(k, l, m, n, l);
    assertSame(r, l);

    assertEqualMatrix(new MatrixIJ(1, 1, 2, 3), k);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(expected, l);
  }

  @Test
  public void testmtxmaddOverM() {
    MatrixIJ tmp = MatrixIJ.mtxm(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mtxm(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mtxmadd(k, l, m, n, m);
    assertSame(r, m);

    assertEqualMatrix(new MatrixIJ(1, 1, 2, 3), k);
    assertEqualMatrix(new MatrixIJ(1, 0, -1, 1), l);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testmtxmaddOverN() {
    MatrixIJ tmp = MatrixIJ.mtxm(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mtxm(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mtxmadd(k, l, m, n, n);
    assertSame(r, n);

    assertEqualMatrix(new MatrixIJ(1, 1, 2, 3), k);
    assertEqualMatrix(new MatrixIJ(1, 0, -1, 1), l);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(expected, n);
  }

  @Test
  public void testmtxmaddMMMMOverM() {
    MatrixIJ tmp = MatrixIJ.mtxm(m, m, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.add(tmp, tmp, new MatrixIJ());

    MatrixIJ r = MatrixIJ.mtxmadd(m, m, m, m, m);
    assertSame(r, m);

    assertEqualMatrix(expected, m);
  }

  @Test
  public void testmtxmadd() {
    MatrixIJ tmp = MatrixIJ.mtxm(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mtxm(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mtxmadd(k, l, m, n, o);
    assertSame(r, o);

    assertEqualMatrix(new MatrixIJ(1, 1, 2, 3), k);
    assertEqualMatrix(new MatrixIJ(1, 0, -1, 1), l);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(expected, o);
  }

  @Test
  public void testNewmtxmadd() {
    MatrixIJ tmp = MatrixIJ.mtxm(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mtxm(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mtxmadd(k, l, m, n);
    assertNotSame(r, k);
    assertNotSame(r, l);
    assertNotSame(r, m);
    assertNotSame(r, n);

    assertEqualMatrix(new MatrixIJ(1, 1, 2, 3), k);
    assertEqualMatrix(new MatrixIJ(1, 0, -1, 1), l);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(expected, r);
  }

  @Test
  public void testmxmtaddOverK() {
    MatrixIJ tmp = MatrixIJ.mxmt(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mxmt(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mxmtadd(k, l, m, n, k);
    assertSame(r, k);

    assertEqualMatrix(new MatrixIJ(1, 0, -1, 1), l);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(expected, k);
  }

  @Test
  public void testmxmtaddOverL() {
    MatrixIJ tmp = MatrixIJ.mxmt(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mxmt(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mxmtadd(k, l, m, n, l);
    assertSame(r, l);

    assertEqualMatrix(new MatrixIJ(1, 1, 2, 3), k);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(expected, l);
  }

  @Test
  public void testmxmtaddOverM() {
    MatrixIJ tmp = MatrixIJ.mxmt(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mxmt(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mxmtadd(k, l, m, n, m);
    assertSame(r, m);

    assertEqualMatrix(new MatrixIJ(1, 1, 2, 3), k);
    assertEqualMatrix(new MatrixIJ(1, 0, -1, 1), l);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testmxmtaddOverN() {
    MatrixIJ tmp = MatrixIJ.mxmt(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mxmt(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mxmtadd(k, l, m, n, n);
    assertSame(r, n);

    assertEqualMatrix(new MatrixIJ(1, 1, 2, 3), k);
    assertEqualMatrix(new MatrixIJ(1, 0, -1, 1), l);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(expected, n);
  }

  @Test
  public void testmxmtaddMMMMOverM() {
    MatrixIJ tmp = MatrixIJ.mxmt(m, m, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.add(tmp, tmp, new MatrixIJ());

    MatrixIJ r = MatrixIJ.mxmtadd(m, m, m, m, m);
    assertSame(r, m);

    assertEqualMatrix(expected, m);
  }

  @Test
  public void testmxmtadd() {
    MatrixIJ tmp = MatrixIJ.mxmt(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mxmt(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mxmtadd(k, l, m, n, o);
    assertSame(r, o);

    assertEqualMatrix(new MatrixIJ(1, 1, 2, 3), k);
    assertEqualMatrix(new MatrixIJ(1, 0, -1, 1), l);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(expected, o);
  }

  @Test
  public void testNewmxmtadd() {
    MatrixIJ tmp = MatrixIJ.mxmt(k, l, new MatrixIJ());
    MatrixIJ expected = MatrixIJ.mxmt(m, n, new MatrixIJ());
    MatrixIJ.add(tmp, expected, expected);

    MatrixIJ r = MatrixIJ.mxmtadd(k, l, m, n);
    assertNotSame(r, k);
    assertNotSame(r, l);
    assertNotSame(r, m);
    assertNotSame(r, n);

    assertEqualMatrix(new MatrixIJ(1, 1, 2, 3), k);
    assertEqualMatrix(new MatrixIJ(1, 0, -1, 1), l);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualMatrix(new MatrixIJ(1, 2, 3, 10), n);
    assertEqualMatrix(expected, r);
  }

  @Test
  public void testCreateUnitizedColumns() {
    MatrixIJ unitizeColumns = valueCon.createUnitizedColumns();
    assertNotSame(unitizeColumns, valueCon);

    double lc1 = Math.sqrt(5.0);
    double lc2 = Math.sqrt(25.0);

    assertComponentEquals(new UnwritableMatrixIJ(-1.0 / lc1, 2.0 / lc1, -3.0 / lc2, 4.0 / lc2),
        unitizeColumns, LOOSE_TOLERANCE);
    assertEqualMatrix(new UnwritableMatrixIJ(-1, 2, -3, 4), valueCon);
  }

  @Test
  public void testUnitizeColumns() {
    MatrixIJ unitizeColumns = valueCon.unitizeColumns();
    assertSame(unitizeColumns, valueCon);

    double lc1 = Math.sqrt(5.0);
    double lc2 = Math.sqrt(25.0);

    assertComponentEquals(new UnwritableMatrixIJ(-1.0 / lc1, 2.0 / lc1, -3.0 / lc2, 4.0 / lc2),
        unitizeColumns, LOOSE_TOLERANCE);

  }

  @Test
  public void testSetToUnitizedColumns() {
    MatrixIJ unitizeColumns = arrayCon.setToUnitizedColumns(valueCon);
    assertSame(unitizeColumns, arrayCon);

    double lc1 = Math.sqrt(5.0);
    double lc2 = Math.sqrt(25.0);

    assertComponentEquals(new UnwritableMatrixIJ(-1.0 / lc1, 2.0 / lc1, -3.0 / lc2, 4.0 / lc2),
        unitizeColumns, LOOSE_TOLERANCE);

    assertEqualMatrix(new UnwritableMatrixIJ(-1, 2, -3, 4), valueCon);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testDiagonalizeSymmetricMatrixNonSymmetricException() {
    MatrixIJ.diagonalizeSymmetricMatrix(new UnwritableMatrixIJ(-1.0, 2.0, 3.0, 1.0), new VectorIJ(),
        new MatrixIJ());
  }

  @Test
  public void testDiagonalizeSymmetricMatrix() {

    VectorIJ values = new VectorIJ();
    MatrixIJ solution = new MatrixIJ();
    MatrixIJ result;

    result = MatrixIJ.diagonalizeSymmetricMatrix(new UnwritableMatrixIJ(4.0, 0.0, 0.0, 6.0), values,
        solution);
    assertSame(result, solution);
    assertEqualVector(new UnwritableVectorIJ(4.0, 6.0), values);
    assertTrue(result.isRotation());
    assertEqualMatrix(MatrixIJ.IDENTITY, result);

    result = MatrixIJ.diagonalizeSymmetricMatrix(new UnwritableMatrixIJ(1.0, -2.0, -2.0, 1.0),
        values, solution);
    assertSame(result, solution);
    assertEqualVector(new UnwritableVectorIJ(-1.0, 3.0), values);
    assertTrue(result.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJ(0.70710678118654746, 0.70710678118654746,
        -0.70710678118654746, 0.70710678118654746), result);

    result = MatrixIJ.diagonalizeSymmetricMatrix(new UnwritableMatrixIJ(-1.0, 2.0, 2.0, -1.0),
        values, solution);
    assertSame(result, solution);
    assertEqualVector(new UnwritableVectorIJ(-3.0, 1.0), values);
    assertTrue(result.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJ(0.70710678118654746, -0.70710678118654746,
        0.70710678118654746, 0.70710678118654746), result);

    result = MatrixIJ.diagonalizeSymmetricMatrix(new UnwritableMatrixIJ(6.0, 2.0, 2.0, -1.0),
        values, solution);
    assertSame(result, solution);
    assertEqualVector(new UnwritableVectorIJ(6.5311288741492746, -1.5311288741492748), values);
    assertTrue(result.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJ(0.96649964876466965, 0.25666793515702424,
        -0.25666793515702424, 0.96649964876466965), result);

    result = MatrixIJ.diagonalizeSymmetricMatrix(
        new UnwritableMatrixIJ(1e300, 20.0e10, 20.0e10, 1.0), values, result);
    assertSame(result, solution);
    assertEqualVector(new UnwritableVectorIJ(1.00000000000000005e300, 1.0), values);
    assertTrue(result.isRotation());
    assertEqualMatrix(
        new UnwritableMatrixIJ(1.0, 1.99999999999999980e-289, -1.99999999999999980E-289, 1.0),
        result);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testSolveQuadraticNoEquationException() {
    MatrixIJ.solveQuadratic(0, 0, 1.0, new VectorIJ());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSolveQuadraticComplexSolutionException() {
    MatrixIJ.solveQuadratic(1.0, 0.0, 1.0, new VectorIJ());
  }

  @Test
  public void testSolveQuadratic() {
    VectorIJ solution = new VectorIJ();
    VectorIJ result;

    result = MatrixIJ.solveQuadratic(1.0, 0.0, -1.0, solution);
    assertSame(result, solution);
    assertEquals(1.0, solution.getI(), 0.0);
    assertEquals(-1.0, solution.getJ(), 0.0);

    result = MatrixIJ.solveQuadratic(1.0, 2.0, 1.0, solution);
    assertSame(result, solution);
    assertEquals(-1.0, solution.getI(), 0.0);
    assertEquals(-1.0, solution.getJ(), 0.0);

    result = MatrixIJ.solveQuadratic(1.0, 4.0, 1.0, solution);
    assertSame(result, solution);
    assertEquals(-0.26794919243112270, solution.getI(), 0.0);
    assertEquals(-3.7320508075688772, solution.getJ(), 0.0);

    result = MatrixIJ.solveQuadratic(1.0, -4.0, 1.0, solution);
    assertSame(result, solution);
    assertEquals(3.7320508075688772, solution.getI(), 0.0);
    assertEquals(0.26794919243112270, solution.getJ(), 0.0);

  }

  static void checkStaticFinalMembers() {

    assertEqualMatrix(new UnwritableMatrixIJ(1, 0, 0, 1), MatrixIJ.IDENTITY);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 1, 1, 1), MatrixIJ.ONES);
    assertEqualMatrix(new UnwritableMatrixIJ(0, 0, 0, 0), MatrixIJ.ZEROS);

  }

}
