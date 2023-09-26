package picante.math.vectorspace;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEqualMatrix;
import static picante.junit.AssertTools.assertEqualVector;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import picante.junit.AssertTools;

public class UnwritableMatrixIJKTest {

  private static final double LOOSE_TOLERANCE = 1.0E-14;

  private UnwritableMatrixIJK valueCon;
  private UnwritableMatrixIJK arrayCon;
  private UnwritableMatrixIJK copyCon;
  private UnwritableMatrixIJK scaleCon;
  private UnwritableMatrixIJK colScaleCon;
  private UnwritableMatrixIJK vectorCon;
  private UnwritableMatrixIJK scaleVectorCon;
  private UnwritableMatrixIJK m;

  private double[][] array;
  private UnwritableVectorIJK ithCol;
  private UnwritableVectorIJK jthCol;
  private UnwritableVectorIJK kthCol;
  private UnwritableVectorIJK ortho1;
  private UnwritableVectorIJK ortho2;
  private UnwritableVectorIJK ortho3;

  private VectorIJK a;
  private VectorIJK b;

  @Before
  public void setUp() throws Exception {

    array = new double[][] {{-1, -2, -3}, {-4, -5, -6}, {-7, -8, -9}};
    ithCol = new UnwritableVectorIJK(1, 2, 3);
    jthCol = new UnwritableVectorIJK(5, 7, 11);
    kthCol = new UnwritableVectorIJK(13, 17, 19);

    valueCon = new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9);
    arrayCon = new UnwritableMatrixIJK(array);
    copyCon = new UnwritableMatrixIJK(valueCon);
    scaleCon = new UnwritableMatrixIJK(2.0, valueCon);
    colScaleCon = new UnwritableMatrixIJK(3.0, 2.0, -1.0, valueCon);
    vectorCon = new UnwritableMatrixIJK(ithCol, jthCol, kthCol);
    scaleVectorCon = new UnwritableMatrixIJK(-1.0, ithCol, 2.0, jthCol, 3.0, kthCol);

    ortho1 = new UnwritableVectorIJK(1, 1, 1).createUnitized();
    ortho2 = VectorIJK.cross(ortho1, new UnwritableVectorIJK(1, 2, 3).createUnitized());
    ortho3 = VectorIJK.cross(ortho1, ortho2).createUnitized();
    m = new UnwritableMatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9);
    a = new VectorIJK(1, 2, 3);
    b = new VectorIJK(4, 5, 6);
  }

  @After
  public void tearDown() throws Exception {
    MatrixIJKTest.checkStaticFinalMembers();
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
  public void testUnwritableMatrixIJKDoubleDoubleDoubleDoubleDoubleDoubleDoubleDoubleDouble() {
    /*
     * Since we generally use the value based constructor in line in the remaining tests of this
     * class, let's check to make sure it functions properly by comparing values directly.
     */
    assertEquals(1.0, valueCon.ii, 0.0);
    assertEquals(2.0, valueCon.ji, 0.0);
    assertEquals(3.0, valueCon.ki, 0.0);
    assertEquals(4.0, valueCon.ij, 0.0);
    assertEquals(5.0, valueCon.jj, 0.0);
    assertEquals(6.0, valueCon.kj, 0.0);
    assertEquals(7.0, valueCon.ik, 0.0);
    assertEquals(8.0, valueCon.jk, 0.0);
    assertEquals(9.0, valueCon.kk, 0.0);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testUnwritableMatrixIJKDoubleArrayArrayException() {
    double[][] badArray = {{1, 2, 3}, {1, 2, 3}};
    new UnwritableMatrixIJK(badArray);
  }

  @Test
  public void testUnwritableMatrixIJKDoubleArrayArray() {
    assertEqualMatrix(new UnwritableMatrixIJK(-1, -4, -7, -2, -5, -8, -3, -6, -9), arrayCon);
    AssertTools.assertEqualDouble(-1, array[0][0]);
    AssertTools.assertEqualDouble(-2, array[0][1]);
    AssertTools.assertEqualDouble(-3, array[0][2]);
    AssertTools.assertEqualDouble(-4, array[1][0]);
    AssertTools.assertEqualDouble(-5, array[1][1]);
    AssertTools.assertEqualDouble(-6, array[1][2]);
    AssertTools.assertEqualDouble(-7, array[2][0]);
    AssertTools.assertEqualDouble(-8, array[2][1]);
    AssertTools.assertEqualDouble(-9, array[2][2]);
  }

  @Test
  public void testUnwritableMatrixIJKUnwritableMatrixIJK() {
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), copyCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test
  public void testUnwritableMatrixIJKDoubleUnwritableMatrixIJK() {
    assertEqualMatrix(new UnwritableMatrixIJK(2, 4, 6, 8, 10, 12, 14, 16, 18), scaleCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test
  public void testUnwritableMatrixIJKDoubleDoubleDoubleUnwritableMatrixIJK() {
    assertEqualMatrix(new UnwritableMatrixIJK(3.0, 6.0, 9.0, 8.0, 10.0, 12.0, -7.0, -8.0, -9.0),
        colScaleCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test
  public void testUnwritableMatrixIJKUnwritableVectorIJKUnwritableVectorIJKUnwritableVectorIJK() {
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 5, 7, 11, 13, 17, 19), vectorCon);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), ithCol);
    assertEqualVector(new UnwritableVectorIJK(5, 7, 11), jthCol);
    assertEqualVector(new UnwritableVectorIJK(13, 17, 19), kthCol);
  }

  @Test
  public void testUnwritableMatrixIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJK() {
    assertEqualMatrix(new UnwritableMatrixIJK(-1, -2, -3, 10, 14, 22, 39, 51, 57), scaleVectorCon);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), ithCol);
    assertEqualVector(new UnwritableVectorIJK(5, 7, 11), jthCol);
    assertEqualVector(new UnwritableVectorIJK(13, 17, 19), kthCol);
  }

  @Test
  public void testMxvOverA() {
    VectorIJK d = m.mxv(a, a);
    assertSame(d, a);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualVector(new VectorIJK(56, 70, 92), a);
  }

  @Test
  public void testMxv() {
    VectorIJK d = m.mxv(a, b);
    assertSame(d, b);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(56, 70, 92), b);
  }

  @Test
  public void testNewMxv() {
    VectorIJK d = m.mxv(a);
    assertNotSame(d, a);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9), m);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(56, 70, 92), d);
  }

  @Test
  public void testMtxvOverA() {
    m = m.createTranspose();
    VectorIJK d = m.mtxv(a, a);
    assertSame(d, a);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9).transpose(), m);
    assertEqualVector(new VectorIJK(56, 70, 92), a);
  }

  @Test
  public void testMtxv() {
    m = m.createTranspose();
    VectorIJK d = m.mtxv(a, b);
    assertSame(d, b);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9).transpose(), m);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(56, 70, 92), b);
  }

  @Test
  public void testNewMtxv() {
    m = m.createTranspose();
    VectorIJK d = m.mtxv(a);
    assertNotSame(d, a);
    assertEqualMatrix(new MatrixIJK(19, 23, 31, 11, 13, 17, 5, 7, 9).transpose(), m);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(56, 70, 92), d);
  }

  @Test
  public void testCreateTranspose() {
    UnwritableMatrixIJK transpose = valueCon.createTranspose();
    assertNotSame(transpose, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 4, 7, 2, 5, 8, 3, 6, 9), transpose);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateUnitizedColumnsZeroIthColException() {
    new UnwritableMatrixIJK(VectorIJK.ZERO, VectorIJK.J, VectorIJK.K).createUnitizedColumns();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateUnitizedColumnsZeroJthColException() {
    new UnwritableMatrixIJK(VectorIJK.I, VectorIJK.ZERO, VectorIJK.K).createUnitizedColumns();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateUnitizedColumnsZeroKthColException() {
    new UnwritableMatrixIJK(VectorIJK.I, VectorIJK.J, VectorIJK.ZERO).createUnitizedColumns();
  }

  @Test
  public void testCreateUnitizedColumns() {
    UnwritableMatrixIJK m = new UnwritableMatrixIJK(10.0, ithCol, 50.0, jthCol, 7.0, kthCol);
    UnwritableMatrixIJK unitizedColM = m.createUnitizedColumns();
    assertNotSame(m, unitizedColM);
    assertEqualMatrix(new UnwritableMatrixIJK(10.0, ithCol, 50.0, jthCol, 7.0, kthCol), m);
    assertEquals(1.0, unitizedColM.getIthColumn(new VectorIJK()).getLength(), LOOSE_TOLERANCE);
    assertEquals(0.0, unitizedColM.getIthColumn(new VectorIJK()).getSeparation(ithCol),
        LOOSE_TOLERANCE);
    assertEquals(1.0, unitizedColM.getJthColumn(new VectorIJK()).getLength(), LOOSE_TOLERANCE);
    assertEquals(0.0, unitizedColM.getJthColumn(new VectorIJK()).getSeparation(jthCol),
        LOOSE_TOLERANCE);
    assertEquals(1.0, unitizedColM.getKthColumn(new VectorIJK()).getLength(), LOOSE_TOLERANCE);
    assertEquals(0.0, unitizedColM.getKthColumn(new VectorIJK()).getSeparation(kthCol),
        LOOSE_TOLERANCE);

  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInverseException() {
    valueCon.createInverse();
  }

  @Test
  public void testCreateInverse() {
    UnwritableMatrixIJK inverse = vectorCon.createInverse();
    assertNotSame(inverse, vectorCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 5, 7, 11, 13, 17, 19), vectorCon);
    UnwritableMatrixIJK product = MatrixIJK.mxm(inverse, vectorCon, new MatrixIJK());
    assertComponentEquals(MatrixIJK.IDENTITY, product, LOOSE_TOLERANCE);

  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInverseDoubleException() {
    valueCon.createInverse(0.00001);
  }

  @Test
  public void testCreateInverseDouble() {
    UnwritableMatrixIJK inverse = vectorCon.createInverse(0.0000001);
    assertNotSame(inverse, vectorCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 5, 7, 11, 13, 17, 19), vectorCon);
    UnwritableMatrixIJK product = MatrixIJK.mxm(inverse, vectorCon, new MatrixIJK());
    assertComponentEquals(MatrixIJK.IDENTITY, product, LOOSE_TOLERANCE);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInvortedIthColException() {
    new UnwritableMatrixIJK(1E-314, ortho1, 30.12, ortho3, 1E10, ortho2).createInvorted();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInvortedJthColException() {
    new UnwritableMatrixIJK(30.12, ortho3, 1E-314, ortho1, 1E10, ortho2).createInvorted();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateInvortedKthColException() {
    new UnwritableMatrixIJK(30.12, ortho3, 1E10, ortho2, 1E-314, ortho1).createInvorted();
  }

  @Test
  public void testCreateInvorted() {
    UnwritableMatrixIJK m = new UnwritableMatrixIJK(12123.0, ortho3, -134E-10, ortho2, 5.0, ortho1);
    UnwritableMatrixIJK inverseM = m.createInvorted();
    assertNotSame(m, inverseM);
    assertEqualMatrix(new UnwritableMatrixIJK(12123.0, ortho3, -134E-10, ortho2, 5.0, ortho1), m);
    assertComponentEquals(MatrixIJK.IDENTITY, MatrixIJK.mxm(m, inverseM), LOOSE_TOLERANCE);
  }

  @Test
  public void testGetII() {
    AssertTools.assertEqualDouble(1.0, valueCon.getII());
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test
  public void testGetJI() {
    AssertTools.assertEqualDouble(2.0, valueCon.getJI());
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test
  public void testGetKI() {
    AssertTools.assertEqualDouble(3.0, valueCon.getKI());
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test
  public void testGetIJ() {
    AssertTools.assertEqualDouble(4.0, valueCon.getIJ());
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test
  public void testGetJJ() {
    AssertTools.assertEqualDouble(5.0, valueCon.getJJ());
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test
  public void testGetKJ() {
    AssertTools.assertEqualDouble(6.0, valueCon.getKJ());
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test
  public void testGetIK() {
    AssertTools.assertEqualDouble(7.0, valueCon.getIK());
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test
  public void testGetJK() {
    AssertTools.assertEqualDouble(8.0, valueCon.getJK());
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test
  public void testGetKK() {
    AssertTools.assertEqualDouble(9.0, valueCon.getKK());
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetRowLowIndexException() {
    valueCon.get(-1, 0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetRowHighIndexException() {
    valueCon.get(3, 0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetColumnLowIndexException() {
    valueCon.get(1, -1);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetColumnHighIndexException() {
    valueCon.get(1, 3);
  }

  @Test
  public void testGet() {
    AssertTools.assertEqualDouble(1.0, valueCon.get(0, 0));
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
    AssertTools.assertEqualDouble(2.0, valueCon.get(1, 0));
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
    AssertTools.assertEqualDouble(3.0, valueCon.get(2, 0));
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
    AssertTools.assertEqualDouble(4.0, valueCon.get(0, 1));
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
    AssertTools.assertEqualDouble(5.0, valueCon.get(1, 1));
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
    AssertTools.assertEqualDouble(6.0, valueCon.get(2, 1));
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
    AssertTools.assertEqualDouble(7.0, valueCon.get(0, 2));
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
    AssertTools.assertEqualDouble(8.0, valueCon.get(1, 2));
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
    AssertTools.assertEqualDouble(9.0, valueCon.get(2, 2));
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test
  public void testGetIthColumn() {
    VectorIJK buffer = new VectorIJK();
    VectorIJK result = valueCon.getIthColumn(buffer);
    assertSame(buffer, result);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), result);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test
  public void testGetJthColumn() {
    VectorIJK buffer = new VectorIJK();
    VectorIJK result = valueCon.getJthColumn(buffer);
    assertSame(buffer, result);
    assertEqualVector(new UnwritableVectorIJK(4, 5, 6), result);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test
  public void testGetKthColumn() {
    VectorIJK buffer = new VectorIJK();
    VectorIJK result = valueCon.getKthColumn(buffer);
    assertSame(buffer, result);
    assertEqualVector(new UnwritableVectorIJK(7, 8, 9), result);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetColumnIndexLowException() {
    valueCon.getColumn(-1, new VectorIJK());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetColumnIndexHighException() {
    valueCon.getColumn(3, new VectorIJK());
  }

  @Test
  public void testGetColumn() {
    VectorIJK buffer = new VectorIJK();
    VectorIJK result = valueCon.getColumn(0, buffer);
    assertSame(buffer, result);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), result);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);

    result = valueCon.getColumn(1, buffer);
    assertSame(buffer, result);
    assertEqualVector(new UnwritableVectorIJK(4, 5, 6), result);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);

    result = valueCon.getColumn(2, buffer);
    assertSame(buffer, result);
    assertEqualVector(new UnwritableVectorIJK(7, 8, 9), result);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
  }

  @Test
  public void testGetDeterminant() {
    AssertTools.assertEqualDouble(0.0, valueCon.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
    AssertTools.assertEqualDouble(0.0, arrayCon.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJK(-1, -4, -7, -2, -5, -8, -3, -6, -9), arrayCon);
    AssertTools.assertEqualDouble(0.0, scaleCon.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJK(2, 4, 6, 8, 10, 12, 14, 16, 18), scaleCon);
    AssertTools.assertEqualDouble(24.0, vectorCon.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 5, 7, 11, 13, 17, 19), vectorCon);
    UnwritableMatrixIJK transpose = vectorCon.createTranspose();
    AssertTools.assertEqualDouble(24.0, transpose.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJK(1, 5, 13, 2, 7, 17, 3, 11, 19), transpose);

    UnwritableMatrixIJK test = new UnwritableMatrixIJK(1, 0, 0, 0, 3, 5, 0, 7, 9);
    AssertTools.assertEqualDouble(-8, test.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJK(1, 0, 0, 0, 3, 5, 0, 7, 9), test);

    test = new UnwritableMatrixIJK(0, 3, 5, 1, 0, 0, 0, 7, 9);
    AssertTools.assertEqualDouble(8, test.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJK(0, 3, 5, 1, 0, 0, 0, 7, 9), test);

    test = new UnwritableMatrixIJK(0, 3, 5, 0, 7, 9, 1, 0, 0);
    AssertTools.assertEqualDouble(-8, test.getDeterminant());
    assertEqualMatrix(new UnwritableMatrixIJK(0, 3, 5, 0, 7, 9, 1, 0, 0), test);

  }

  @Test
  public void testGetTrace() {
    AssertTools.assertEqualDouble(27.0, vectorCon.getTrace());
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 5, 7, 11, 13, 17, 19), vectorCon);
  }

  @Test
  public void testIsRotation() {
    assertFalse(valueCon.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
    assertTrue(MatrixIJK.IDENTITY.isRotation());

    UnwritableVectorIJK a = new UnwritableVectorIJK(1, 1, 1).createUnitized();
    UnwritableVectorIJK b = new UnwritableVectorIJK(1, 2, 1);
    UnwritableVectorIJK c = VectorIJK.cross(a, b, new VectorIJK()).unitize();
    b = VectorIJK.cross(c, a, new VectorIJK()).unitize();

    UnwritableMatrixIJK rotMat = new UnwritableMatrixIJK(a, b, c);

    assertTrue(rotMat.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJK(a, b, c), rotMat);

    UnwritableMatrixIJK mat = new UnwritableMatrixIJK(0.5, a, 1.0, b, 1.0, c);
    assertFalse(mat.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJK(0.5, a, 1.0, b, 1.0, c), mat);

    mat = new UnwritableMatrixIJK(1.5, a, 1.0, b, 1.0, c);
    assertFalse(mat.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJK(1.5, a, 1.0, b, 1.0, c), mat);

    mat = new UnwritableMatrixIJK(1.0, a, 0.5, b, 1.0, c);
    assertFalse(mat.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJK(1.0, a, 0.5, b, 1.0, c), mat);

    mat = new UnwritableMatrixIJK(1.0, a, 1.5, b, 1.0, c);
    assertFalse(mat.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJK(1.0, a, 1.5, b, 1.0, c), mat);

    mat = new UnwritableMatrixIJK(1.0, a, 1.0, b, 0.5, c);
    assertFalse(mat.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJK(1.0, a, 1.0, b, 0.5, c), mat);

    mat = new UnwritableMatrixIJK(1.0, a, 1.0, b, 1.5, c);
    assertFalse(mat.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJK(1.0, a, 1.0, b, 1.5, c), mat);

    b = VectorIJK.rotate(b, c, Math.toRadians(45.0), new VectorIJK());
    mat = new UnwritableMatrixIJK(a, b, c);
    assertFalse(mat.isRotation());
    assertEqualMatrix(new UnwritableMatrixIJK(a, b, c), mat);
  }

  @Test
  public void testIsRotationDoubleDouble() {
    double normTol = 1.0E-14;
    double detTol = 1.0E-14;
    assertFalse(valueCon.isRotation(normTol, detTol));
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
    assertTrue(MatrixIJK.IDENTITY.isRotation(normTol, detTol));

    UnwritableVectorIJK a = new UnwritableVectorIJK(1, 1, 1).createUnitized();
    UnwritableVectorIJK b = new UnwritableVectorIJK(1, 2, 1);
    UnwritableVectorIJK c = VectorIJK.cross(a, b, new VectorIJK()).unitize();
    b = VectorIJK.cross(c, a, new VectorIJK()).unitize();

    UnwritableMatrixIJK rotMat = new UnwritableMatrixIJK(a, b, c);

    assertTrue(rotMat.isRotation(normTol, detTol));
    assertEqualMatrix(new UnwritableMatrixIJK(a, b, c), rotMat);

    UnwritableMatrixIJK mat = new UnwritableMatrixIJK(1.0 - 1.0E-13, a, 1.0, b, 1.0, c);
    assertFalse(mat.isRotation(normTol, detTol));
    assertEqualMatrix(new UnwritableMatrixIJK(1.0 - 1.0E-13, a, 1.0, b, 1.0, c), mat);

    mat = new UnwritableMatrixIJK(1.0 + 1.0E-13, a, 1.0, b, 1.0, c);
    assertFalse(mat.isRotation(normTol, detTol));
    assertEqualMatrix(new UnwritableMatrixIJK(1.0 + 1.0E-13, a, 1.0, b, 1.0, c), mat);

    mat = new UnwritableMatrixIJK(1.0, a, 1.0 - 1.0E-13, b, 1.0, c);
    assertFalse(mat.isRotation(normTol, detTol));
    assertEqualMatrix(new UnwritableMatrixIJK(1.0, a, 1.0 - 1.0E-13, b, 1.0, c), mat);

    mat = new UnwritableMatrixIJK(1.0, a, 1.0 + 1.0E-13, b, 1.0, c);
    assertFalse(mat.isRotation(normTol, detTol));
    assertEqualMatrix(new UnwritableMatrixIJK(1.0, a, 1.0 + 1.0E-13, b, 1.0, c), mat);

    mat = new UnwritableMatrixIJK(1.0, a, 1.0, b, 1.0 - 1.0E-13, c);
    assertFalse(mat.isRotation(normTol, detTol));
    assertEqualMatrix(new UnwritableMatrixIJK(1.0, a, 1.0, b, 1.0 - 1.0E-13, c), mat);

    mat = new UnwritableMatrixIJK(1.0, a, 1.0, b, 1.0 + 1.0E-13, c);
    assertFalse(mat.isRotation(normTol, detTol));
    assertEqualMatrix(new UnwritableMatrixIJK(1.0, a, 1.0, b, 1.0 + 1.0E-13, c), mat);

    b = VectorIJK.rotate(b, c, Math.toRadians(1.0E-5), new VectorIJK());
    mat = new UnwritableMatrixIJK(a, b, c);
    assertFalse(mat.isRotation(normTol, detTol));
    assertEqualMatrix(new UnwritableMatrixIJK(a, b, c), mat);
  }

  @Test
  public void testCopyOf() {

    UnwritableMatrixIJK unwritable = new UnwritableMatrixIJK(MatrixIJK.ONES);
    UnwritableMatrixIJK notUnwritable = new UnwritableMatrixIJK(MatrixIJK.ZEROS) {};

    UnwritableMatrixIJK result = UnwritableMatrixIJK.copyOf(unwritable);
    assertSame(result, unwritable);

    result = UnwritableMatrixIJK.copyOf(notUnwritable);
    assertNotSame(result, notUnwritable);
    assertEquals(result, notUnwritable);
    assertEquals(UnwritableMatrixIJK.class, result.getClass());

  }

  @Test
  public void testEqualsObject() {
    assertTrue(valueCon.equals(copyCon));
    assertNotSame(valueCon, copyCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), copyCon);

    assertTrue(valueCon.equals(valueCon));
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);

    assertFalse(valueCon.equals(null));
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);

    assertFalse(valueCon.equals(new String()));
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);

    assertFalse(valueCon.equals(scaleCon));
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(2, 4, 6, 8, 10, 12, 14, 16, 18), scaleCon);

    UnwritableMatrixIJK subClass = new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9) {};

    assertTrue(valueCon.equals(subClass));
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), subClass);

    assertTrue(subClass.equals(valueCon));
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9), subClass);

  }

  @Test
  public void testToString() {
    assertEquals("[1.0,2.0,3.0;4.0,5.0,6.0;7.0,8.0,9.0]", valueCon.toString());
  }

}
