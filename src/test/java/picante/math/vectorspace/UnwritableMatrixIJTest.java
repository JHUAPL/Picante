package picante.math.vectorspace;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEqualDouble;
import static picante.junit.AssertTools.assertEqualMatrix;
import static picante.junit.AssertTools.assertEqualVector;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class UnwritableMatrixIJTest {

  private static final double LOOSE_TOLERANCE = 1.0E-14;

  private UnwritableMatrixIJ valueCon;
  private UnwritableMatrixIJ arrayCon;
  private UnwritableMatrixIJ copyCon;
  private UnwritableMatrixIJ scaleCon;
  private UnwritableMatrixIJ colScaleCon;
  private UnwritableMatrixIJ vectorCon;
  private UnwritableMatrixIJ scaleVectorCon;
  private UnwritableMatrixIJ zeroDetCon;
  private UnwritableMatrixIJ m;

  private double[][] array;
  private UnwritableVectorIJ ithCol;
  private UnwritableVectorIJ jthCol;
  private UnwritableVectorIJ ortho1;
  private UnwritableVectorIJ ortho2;

  private VectorIJ a;
  private VectorIJ b;

  @Before
  public void setUp() throws Exception {

    array = new double[][] {{-1, -2}, {-3, -4}};
    ithCol = new UnwritableVectorIJ(1, 2);
    jthCol = new UnwritableVectorIJ(5, 7);

    valueCon = new UnwritableMatrixIJ(1, 2, 3, 4);
    arrayCon = new UnwritableMatrixIJ(array);
    copyCon = new UnwritableMatrixIJ(valueCon);
    scaleCon = new UnwritableMatrixIJ(2.0, valueCon);
    colScaleCon = new UnwritableMatrixIJ(3.0, -2.0, valueCon);
    vectorCon = new UnwritableMatrixIJ(ithCol, jthCol);
    scaleVectorCon = new UnwritableMatrixIJ(-1.0, ithCol, 2.0, jthCol);
    zeroDetCon = new UnwritableMatrixIJ(1.5, 2, 3, 4);

    ortho1 = new UnwritableVectorIJ(1, 1).createUnitized();
    ortho2 = new UnwritableVectorIJ(-1, 1).createUnitized();
    m = new UnwritableMatrixIJ(19, 23, 31, 11);
    a = new VectorIJ(1, 2);
    b = new VectorIJ(4, 5);
  }

  @After
  public void tearDown() throws Exception {
    MatrixIJTest.checkStaticFinalMembers();
  }

  @Test
  public void testHashCode() {

    /*
     * Simply check that matrices that are equal, but are different instances, have equal hashcodes.
     * This test is not definitive, as it's possible that these two matrices have the same hashcode,
     * but other equal matrices may not.
     */
    assertEquals(valueCon.hashCode(), copyCon.hashCode());
    assertNotSame(valueCon, copyCon);
  }

  @Test
  public void testUnwritableMatrixIJDoubleDoubleDoubleDouble() {
    /*
     * Since we generally use the value based constructor in line in the remaining tests of this
     * class, let's check to make sure it functions properly by comparing values directly.
     */
    assertEquals(1.0, valueCon.ii, 0.0);
    assertEquals(2.0, valueCon.ji, 0.0);
    assertEquals(3.0, valueCon.ij, 0.0);
    assertEquals(4.0, valueCon.jj, 0.0);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testUnwritableMatrixIJDoubleArrayArrayException() {
    double[][] badArray = {{1, 2, 3}};
    new UnwritableMatrixIJ(badArray);
  }

  @Test
  public void testUnwritableMatrixIJDoubleArrayArray() {
    assertEqualMatrix(new UnwritableMatrixIJ(-1, -3, -2, -4), arrayCon);
    assertEqualDouble(-1, array[0][0]);
    assertEqualDouble(-2, array[0][1]);
    assertEqualDouble(-3, array[1][0]);
    assertEqualDouble(-4, array[1][1]);
  }

  @Test
  public void testUnwritableMatrixIJUnwritableMatrixIJ() {
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), copyCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
  }

  @Test
  public void testUnwritableMatrixIJDoubleUnwritableMatrixIJ() {
    assertEqualMatrix(new UnwritableMatrixIJ(2, 4, 6, 8), scaleCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
  }

  @Test
  public void testUnwritableMatrixIJDoubleDoubleUnwritableMatrixIJ() {
    assertEqualMatrix(new UnwritableMatrixIJ(3.0, 6.0, -6, -8), colScaleCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
  }

  @Test
  public void testUnwritableMatrixIJUnwritableVectorIJUnwritableVectorIJ() {
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 5, 7), vectorCon);
    assertEqualVector(new UnwritableVectorIJ(1, 2), ithCol);
    assertEqualVector(new UnwritableVectorIJ(5, 7), jthCol);
  }

  @Test
  public void testUnwritableMatrixIJDoubleUnwritableVectorIJDoubleUnwritableVectorIJ() {
    assertEqualMatrix(new UnwritableMatrixIJ(-1, -2, 10, 14), scaleVectorCon);
    assertEqualVector(new UnwritableVectorIJ(1, 2), ithCol);
    assertEqualVector(new UnwritableVectorIJ(5, 7), jthCol);
  }

  @Test
  public void testMxvOverA() {
    VectorIJ d = m.mxv(a, a);
    assertSame(d, a);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualVector(new VectorIJ(81, 45), a);
  }

  @Test
  public void testMxv() {
    VectorIJ d = m.mxv(a, b);
    assertSame(d, b);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(81, 45), b);
  }

  @Test
  public void testNewMxv() {
    VectorIJ d = m.mxv(a);
    assertNotSame(d, a);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11), m);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(81, 45), d);
  }

  @Test
  public void testMtxvOverA() {
    m = m.createTranspose();
    VectorIJ d = m.mtxv(a, a);
    assertSame(d, a);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11).transpose(), m);
    assertEqualVector(new VectorIJ(81, 45), a);
  }

  @Test
  public void testMtxv() {
    m = m.createTranspose();
    VectorIJ d = m.mtxv(a, b);
    assertSame(d, b);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11).transpose(), m);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(81, 45), b);
  }

  @Test
  public void testNewMtxv() {
    m = m.createTranspose();
    VectorIJ d = m.mtxv(a);
    assertNotSame(d, a);
    assertEqualMatrix(new MatrixIJ(19, 23, 31, 11).transpose(), m);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(81, 45), d);
  }

  @Test
  public void testCreateTranspose() {
    UnwritableMatrixIJ transpose = valueCon.createTranspose();
    assertNotSame(transpose, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 3, 2, 4), transpose);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateUnitizedColumnsZeroIthColException() {
    new UnwritableMatrixIJ(VectorIJ.ZERO, VectorIJ.J).createUnitizedColumns();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateUnitizedColumnsZeroJthColException() {
    new UnwritableMatrixIJ(VectorIJ.I, VectorIJ.ZERO).createUnitizedColumns();
  }

  @Test
  public void testCreateUnitizedColumns() {
    UnwritableMatrixIJ m = new UnwritableMatrixIJ(10.0, ithCol, 50.0, jthCol);
    UnwritableMatrixIJ unitizedColM = m.createUnitizedColumns();
    assertNotSame(m, unitizedColM);
    assertEqualMatrix(new UnwritableMatrixIJ(10.0, ithCol, 50.0, jthCol), m);
    assertEquals(1.0, unitizedColM.getIthColumn(new VectorIJ()).getLength(), LOOSE_TOLERANCE);
    assertEquals(0.0, unitizedColM.getIthColumn(new VectorIJ()).getSeparation(ithCol),
        LOOSE_TOLERANCE);
    assertEquals(1.0, unitizedColM.getJthColumn(new VectorIJ()).getLength(), LOOSE_TOLERANCE);
    assertEquals(0.0, unitizedColM.getJthColumn(new VectorIJ()).getSeparation(jthCol),
        LOOSE_TOLERANCE);

  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInverseException() {
    zeroDetCon.createInverse();
  }

  @Test
  public void testCreateInverse() {
    UnwritableMatrixIJ inverse = vectorCon.createInverse();
    assertNotSame(inverse, vectorCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 5, 7), vectorCon);
    UnwritableMatrixIJ product = MatrixIJ.mxm(inverse, vectorCon, new MatrixIJ());
    assertComponentEquals(MatrixIJ.IDENTITY, product, LOOSE_TOLERANCE);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInverseDoubleException() {
    zeroDetCon.createInverse(0.00001);
  }

  @Test
  public void testCreateInverseDouble() {
    UnwritableMatrixIJ inverse = vectorCon.createInverse(0.0000001);
    assertNotSame(inverse, vectorCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 5, 7), vectorCon);
    UnwritableMatrixIJ product = MatrixIJ.mxm(inverse, vectorCon, new MatrixIJ());
    assertComponentEquals(MatrixIJ.IDENTITY, product, LOOSE_TOLERANCE);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInvortedIthColException() {
    new UnwritableMatrixIJ(1E-314, ortho1, 1E10, ortho2).createInvorted();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInvortedJthColException() {
    new UnwritableMatrixIJ(1E10, ortho2, 1E-314, ortho1).createInvorted();
  }

  @Test
  public void testCreateInvorted() {
    UnwritableMatrixIJ m = new UnwritableMatrixIJ(-134E-10, ortho2, 5.0, ortho1);
    UnwritableMatrixIJ inverseM = m.createInvorted();
    assertNotSame(m, inverseM);
    assertEqualMatrix(new UnwritableMatrixIJ(-134E-10, ortho2, 5.0, ortho1), m);
    assertComponentEquals(MatrixIJ.IDENTITY, MatrixIJ.mxm(m, inverseM), LOOSE_TOLERANCE);
  }

  @Test
  public void testGetII() {
    assertEqualDouble(1.0, valueCon.getII());
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
  }

  @Test
  public void testGetJI() {
    assertEqualDouble(2.0, valueCon.getJI());
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
  }

  @Test
  public void testGetIJ() {
    assertEqualDouble(3.0, valueCon.getIJ());
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
  }

  @Test
  public void testGetJJ() {
    assertEqualDouble(4.0, valueCon.getJJ());
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetRowLowIndexException() {
    valueCon.get(-1, 0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetRowHighIndexException() {
    valueCon.get(2, 0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetColumnLowIndexException() {
    valueCon.get(1, -1);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetColumnHighIndexException() {
    valueCon.get(1, 2);
  }

  @Test
  public void testGet() {
    assertEqualDouble(1.0, valueCon.get(0, 0));
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
    assertEqualDouble(2.0, valueCon.get(1, 0));
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
    assertEqualDouble(3.0, valueCon.get(0, 1));
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
    assertEqualDouble(4.0, valueCon.get(1, 1));
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
  }

  @Test
  public void testGetIthColumn() {
    VectorIJ buffer = new VectorIJ();
    VectorIJ result = valueCon.getIthColumn(buffer);
    assertSame(buffer, result);
    assertEqualVector(new UnwritableVectorIJ(1, 2), result);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
  }

  @Test
  public void testGetJthColumn() {
    VectorIJ buffer = new VectorIJ();
    VectorIJ result = valueCon.getJthColumn(buffer);
    assertSame(buffer, result);
    assertEqualVector(new UnwritableVectorIJ(3, 4), result);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetColumnIndexLowException() {
    valueCon.getColumn(-1, new VectorIJ());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetColumnIndexHighException() {
    valueCon.getColumn(2, new VectorIJ());
  }

  @Test
  public void testGetColumn() {
    VectorIJ buffer = new VectorIJ();
    VectorIJ result = valueCon.getColumn(0, buffer);
    assertSame(buffer, result);
    assertEqualVector(new UnwritableVectorIJ(1, 2), result);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);

    result = valueCon.getColumn(1, buffer);
    assertSame(buffer, result);
    assertEqualVector(new UnwritableVectorIJ(3, 4), result);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);

  }

  @Test
  public void testGetDeterminant() {
    assertEqualDouble(-2.0, valueCon.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
    assertEqualDouble(-2.0, arrayCon.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJ(-1, -3, -2, -4), arrayCon);
    assertEqualDouble(-8.0, scaleCon.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJ(2, 4, 6, 8), scaleCon);
    assertEqualDouble(-3.0, vectorCon.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 5, 7), vectorCon);
    assertEqualDouble(0, zeroDetCon.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJ(1.5, 2, 3, 4), zeroDetCon);
    UnwritableMatrixIJ transpose = vectorCon.createTranspose();
    assertEqualDouble(-3.0, transpose.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJ(1, 5, 2, 7), transpose);

    UnwritableMatrixIJ test = new UnwritableMatrixIJ(1, 1, 0, 3);
    assertEqualDouble(3, test.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJ(1, 1, 0, 3), test);

    test = new UnwritableMatrixIJ(0, 3, 5, 1);
    assertEqualDouble(-15, test.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJ(0, 3, 5, 1), test);

    test = new UnwritableMatrixIJ(0, 3, 0, 5);
    assertEqualDouble(0, test.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJ(0, 3, 0, 5), test);
  }

  @Test
  public void testGetTrace() {
    assertEqualDouble(8.0, vectorCon.getTrace());
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 5, 7), vectorCon);
  }

  @Test
  public void testIsRotation() {
    assertFalse(valueCon.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
    assertTrue(MatrixIJ.IDENTITY.isRotation());

    double theta = -Math.PI * 2.0;
    double step = .0001;

    while (theta <= Math.PI * 2.0) {

      UnwritableMatrixIJ rotMat = new UnwritableMatrixIJ(Math.cos(theta), Math.sin(theta),
          -Math.sin(theta), Math.cos(theta));

      assertTrue(rotMat.isRotation());
      assertEqualMatrix(new UnwritableMatrixIJ(Math.cos(theta), Math.sin(theta), -Math.sin(theta),
          Math.cos(theta)), rotMat);

      theta += step;

    }

    theta = 1.2525;

    UnwritableMatrixIJ mat = new UnwritableMatrixIJ(2.1 * Math.cos(theta), Math.sin(theta),
        -Math.sin(theta), Math.cos(theta));
    assertFalse(mat.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJ(2.1 * Math.cos(theta), Math.sin(theta),
        -Math.sin(theta), Math.cos(theta)), mat);

    mat = new UnwritableMatrixIJ(Math.cos(theta), 2.1 * Math.sin(theta), -Math.sin(theta),
        Math.cos(theta));

    assertFalse(mat.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJ(Math.cos(theta), 2.1 * Math.sin(theta),
        -Math.sin(theta), Math.cos(theta)), mat);

    mat = new UnwritableMatrixIJ(Math.cos(theta), Math.sin(theta), -2.1 * Math.sin(theta),
        Math.cos(theta));
    assertFalse(mat.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJ(Math.cos(theta), Math.sin(theta),
        -2.1 * Math.sin(theta), Math.cos(theta)), mat);

    mat = new UnwritableMatrixIJ(Math.cos(theta), Math.sin(theta), -Math.sin(theta),
        2.1 * Math.cos(theta));
    assertFalse(mat.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJ(Math.cos(theta), Math.sin(theta), -Math.sin(theta),
        2.1 * Math.cos(theta)), mat);

  }

  @Test
  public void testIsRotationDoubleDouble() {
    double normTol = 1.0E-14;
    double detTol = 1.0E-14;
    assertFalse(valueCon.isRotation(normTol, detTol));
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
    assertTrue(MatrixIJK.IDENTITY.isRotation(normTol, detTol));

    UnwritableVectorIJ a = new UnwritableVectorIJ(1, 0);
    UnwritableVectorIJ b = new UnwritableVectorIJ(0, 1);

    UnwritableMatrixIJ rotMat = new UnwritableMatrixIJ(a, b);

    assertTrue(rotMat.isRotation(normTol, detTol));
    assertEqualMatrix(new UnwritableMatrixIJ(a, b), rotMat);

    UnwritableMatrixIJ mat = new UnwritableMatrixIJ(1.0 - 1.0E-13, a, 1.0, b);
    assertFalse(mat.isRotation(normTol, detTol));
    assertEqualMatrix(new UnwritableMatrixIJ(1.0 - 1.0E-13, a, 1.0, b), mat);

    mat = new UnwritableMatrixIJ(1.0 + 1.0E-13, a, 1.0, b);
    assertFalse(mat.isRotation(normTol, detTol));
    assertEqualMatrix(new UnwritableMatrixIJ(1.0 + 1.0E-13, a, 1.0, b), mat);

    mat = new UnwritableMatrixIJ(1.0, a, 1.0 - 1.0E-13, b);
    assertFalse(mat.isRotation(normTol, detTol));
    assertEqualMatrix(new UnwritableMatrixIJ(1.0, a, 1.0 - 1.0E-13, b), mat);

    mat = new UnwritableMatrixIJ(1.0, a, 1.0 + 1.0E-13, b);
    assertFalse(mat.isRotation(normTol, detTol));
    assertEqualMatrix(new UnwritableMatrixIJ(1.0, a, 1.0 + 1.0E-13, b), mat);

    mat = new UnwritableMatrixIJ(1.0, a, 1.0, b);
    assertTrue(mat.isRotation(normTol, detTol));
    assertEqualMatrix(new UnwritableMatrixIJ(1.0, a, 1.0, b), mat);

  }

  @Test
  public void testCopyOf() {

    UnwritableMatrixIJ unwritable = new UnwritableMatrixIJ(MatrixIJ.ONES);
    UnwritableMatrixIJ notUnwritable = new UnwritableMatrixIJ(MatrixIJ.IDENTITY) {};

    UnwritableMatrixIJ result = UnwritableMatrixIJ.copyOf(unwritable);
    assertSame(result, unwritable);

    result = UnwritableMatrixIJ.copyOf(notUnwritable);
    assertNotSame(result, notUnwritable);
    assertEquals(result, notUnwritable);
    assertEquals(UnwritableMatrixIJ.class, result.getClass());

  }

  @Test
  public void testEqualsObject() {
    assertTrue(valueCon.equals(copyCon));
    assertNotSame(valueCon, copyCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), copyCon);

    assertTrue(valueCon.equals(valueCon));
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);

    assertFalse(valueCon.equals(null));
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);

    assertFalse(valueCon.equals(new String()));
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);

    assertFalse(valueCon.equals(scaleCon));
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(2, 4, 6, 8), scaleCon);

    UnwritableMatrixIJ subClass = new UnwritableMatrixIJ(1, 2, 3, 4) {};

    assertTrue(valueCon.equals(subClass));
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), subClass);

    assertTrue(subClass.equals(valueCon));
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), valueCon);
    assertEqualMatrix(new UnwritableMatrixIJ(1, 2, 3, 4), subClass);

  }

  @Test
  public void testToString() {
    assertEquals("[1.0,2.0;3.0,4.0]", valueCon.toString());
  }

}
