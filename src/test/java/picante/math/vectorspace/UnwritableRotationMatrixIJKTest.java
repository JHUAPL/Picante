package picante.math.vectorspace;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEqualMatrix;
import static picante.junit.AssertTools.assertEqualVector;
import static picante.junit.AssertTools.assertEquivalentMatrix;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import picante.junit.AssertTools;

public class UnwritableRotationMatrixIJKTest {

  private static final double LOOSE_TOLERANCE = 1.0E-14;
  private static final double TIGHT_TOLERANCE = 1.5E-16;

  private static final double ROOT3BY2 = Math.sqrt(3) / 2.0;
  private static final double ROOT2RECIP = 1.0 / Math.sqrt(2);

  private UnwritableRotationMatrixIJK valueCon;
  private UnwritableRotationMatrixIJK arrayCon;
  private UnwritableRotationMatrixIJK copyCon;
  private UnwritableRotationMatrixIJK copyRotCon;
  private UnwritableRotationMatrixIJK scaleCon;
  private UnwritableRotationMatrixIJK scaleColCon;
  private UnwritableRotationMatrixIJK vectorCon;
  private UnwritableRotationMatrixIJK scaleVectorCon;

  private double[][] array;
  private UnwritableMatrixIJK matrix;
  private UnwritableMatrixIJK scaledMatrix;
  private UnwritableMatrixIJK scaledColMatrix;
  private UnwritableVectorIJK ithColumn;
  private UnwritableVectorIJK jthColumn;
  private UnwritableVectorIJK kthColumn;
  private UnwritableVectorIJK scaledIthColumn;
  private UnwritableVectorIJK scaledJthColumn;
  private UnwritableVectorIJK scaledKthColumn;

  @Before
  public void setUp() throws Exception {

    array = new double[][] {{0.5, ROOT3BY2, 0}, {-ROOT3BY2, 0.5, 0.0}, {0, 0, 1}};
    matrix = new UnwritableMatrixIJK(-0.23795296, -0.95387879, 0.18301270, 0.94505974, -0.27086608,
        -0.18301270, 0.22414387, 0.12940952, 0.96592583);
    scaledMatrix = new UnwritableMatrixIJK(5.0, matrix);
    scaledColMatrix = new UnwritableMatrixIJK(2.0, -3.0, 4.0, matrix);

    ithColumn = new UnwritableVectorIJK(-0.17677670, -0.91855865, 0.35355339);
    jthColumn = new UnwritableVectorIJK(0.88388348, -0.30618622, -0.35355339);
    kthColumn = new UnwritableVectorIJK(0.43301270, 0.25000000, 0.86602540);
    scaledIthColumn = new UnwritableVectorIJK(11.0, ithColumn);
    scaledJthColumn = new UnwritableVectorIJK(-5.0, jthColumn);
    scaledKthColumn = new UnwritableVectorIJK(3.0, kthColumn);

    valueCon = new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0, -ROOT2RECIP, 0,
        ROOT2RECIP);
    arrayCon = new UnwritableRotationMatrixIJK(array);
    copyCon = new UnwritableRotationMatrixIJK(matrix);
    copyRotCon = new UnwritableRotationMatrixIJK(valueCon);
    scaleCon = new UnwritableRotationMatrixIJK(1.0 / 5.0, scaledMatrix);
    scaleColCon = new UnwritableRotationMatrixIJK(0.5, -1.0 / 3.0, 1.0 / 4.0, scaledColMatrix);
    vectorCon = new UnwritableRotationMatrixIJK(ithColumn, jthColumn, kthColumn);
    scaleVectorCon = new UnwritableRotationMatrixIJK(1.0 / 11.0, scaledIthColumn, -1.0 / 5.0,
        scaledJthColumn, 1.0 / 3.0, scaledKthColumn);

  }

  @After
  public void tearDown() throws Exception {
    RotationMatrixIJKTest.checkStaticFinalMembers();
  }

  @Test(expected = IllegalArgumentException.class)
  public void testUnwritableRotationMatrixIJKDoubleDoubleDoubleDoubleDoubleDoubleDoubleDoubleDoubleException() {
    new UnwritableRotationMatrixIJK(scaledColMatrix.ii, scaledColMatrix.ji, scaledColMatrix.ki,
        scaledColMatrix.ij, scaledColMatrix.jj, scaledColMatrix.kj, scaledColMatrix.ik,
        scaledColMatrix.jk, scaledColMatrix.kk);
  }

  @Test
  public void testUnwritableRotationMatrixIJKDoubleDoubleDoubleDoubleDoubleDoubleDoubleDoubleDouble() {
    /*
     * Since we generally use the value based constructor in line in the remaining tests of this
     * class, let's check to make sure it functions properly by comparing values directly.
     */
    AssertTools.assertEqualDouble(ROOT2RECIP, valueCon.ii);
    AssertTools.assertEqualDouble(0, valueCon.ji);
    AssertTools.assertEqualDouble(ROOT2RECIP, valueCon.ki);
    AssertTools.assertEqualDouble(0, valueCon.ij);
    AssertTools.assertEqualDouble(1, valueCon.jj);
    AssertTools.assertEqualDouble(0, valueCon.kj);
    AssertTools.assertEqualDouble(-ROOT2RECIP, valueCon.ik);
    AssertTools.assertEqualDouble(0, valueCon.jk);
    AssertTools.assertEqualDouble(ROOT2RECIP, valueCon.kk);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testUnwritableRotationMatrixIJKDoubleArrayArrayIndexException() {
    double[][] badArray = {{1, 0, 0}, {0, 0, 1}};
    new UnwritableRotationMatrixIJK(badArray);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testUnwritableRotationMatrixIJKDoubleArrayArrayException() {
    double[][] badArray = {{scaledColMatrix.ii, scaledColMatrix.ij, scaledColMatrix.ik},
        {scaledColMatrix.ji, scaledColMatrix.jj, scaledColMatrix.jk},
        {scaledColMatrix.ki, scaledColMatrix.kj, scaledColMatrix.kk}};
    new UnwritableRotationMatrixIJK(badArray);
  }

  @Test
  public void testUnwritableRotationMatrixIJKDoubleArrayArray() {
    assertEqualMatrix(new UnwritableRotationMatrixIJK(0.5, -ROOT3BY2, 0, ROOT3BY2, 0.5, 0, 0, 0, 1),
        arrayCon);
    AssertTools.assertEqualDouble(0.5, array[0][0]);
    AssertTools.assertEqualDouble(-ROOT3BY2, array[1][0]);
    AssertTools.assertEqualDouble(0, array[2][0]);
    AssertTools.assertEqualDouble(ROOT3BY2, array[0][1]);
    AssertTools.assertEqualDouble(0.5, array[1][1]);
    AssertTools.assertEqualDouble(0.0, array[2][1]);
    AssertTools.assertEqualDouble(0.0, array[0][2]);
    AssertTools.assertEqualDouble(0.0, array[1][2]);
    AssertTools.assertEqualDouble(1.0, array[2][2]);
  }

  @Test
  public void testUnwritableRotationMatrixIJKUnwritableRotationMatrixIJK() {
    assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
        -ROOT2RECIP, 0, ROOT2RECIP), copyRotCon);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
        -ROOT2RECIP, 0, ROOT2RECIP), valueCon);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testUnwritableRotationMatrixIJKUnwritableMatrixIJKException() {
    new UnwritableRotationMatrixIJK(scaledColMatrix);
  }

  @Test
  public void testUnwritableRotationMatrixIJKUnwritableMatrixIJK() {
    assertEqualMatrix(new UnwritableRotationMatrixIJK(-0.23795296, -0.95387879, 0.18301270,
        0.94505974, -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583), copyCon);
    assertEqualMatrix(new UnwritableMatrixIJK(-0.23795296, -0.95387879, 0.18301270, 0.94505974,
        -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583), matrix);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testUnwritableRotationMatrixIJKDoubleUnwritableMatrixIJKException() {
    new UnwritableRotationMatrixIJK(10.0, scaledColMatrix);
  }

  @Test
  public void testUnwritableRotationMatrixIJKDoubleUnwritableMatrixIJK() {
    assertEquivalentMatrix(new UnwritableRotationMatrixIJK(-0.23795296, -0.95387879, 0.18301270,
        0.94505974, -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583), scaleCon);
    assertEqualMatrix(
        new UnwritableMatrixIJK(5.0, new UnwritableMatrixIJK(-0.23795296, -0.95387879, 0.18301270,
            0.94505974, -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583)),
        scaledMatrix);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testUnwritableRotationMatrixIJKDoubleDoubleDoubleUnwritableMatrixIJKException() {
    new UnwritableRotationMatrixIJK(10.0, 12.0, 13.0, matrix);
  }

  @Test
  public void testUnwritableRotationMatrixIJKDoubleDoubleDoubleUnwritableMatrixIJK() {
    assertEquivalentMatrix(new UnwritableRotationMatrixIJK(-0.23795296, -0.95387879, 0.18301270,
        0.94505974, -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583), scaleColCon);
    assertEqualMatrix(
        new UnwritableMatrixIJK(2.0, -3.0, 4.0, new UnwritableMatrixIJK(-0.23795296, -0.95387879,
            0.18301270, 0.94505974, -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583)),
        scaledColMatrix);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testUnwritableRotationMatrixIJKUnwritableVectorIJKUnwritableVectorIJKUnwritableVectorIJKException() {
    new UnwritableRotationMatrixIJK(scaledIthColumn, scaledJthColumn, scaledKthColumn);
  }

  @Test
  public void testUnwritableRotationMatrixIJKUnwritableVectorIJKUnwritableVectorIJKUnwritableVectorIJK() {
    assertEqualMatrix(new UnwritableRotationMatrixIJK(-0.17677670, -0.91855865, 0.35355339,
        0.88388348, -0.30618622, -0.35355339, 0.43301270, 0.25000000, 0.86602540), vectorCon);
    assertEqualVector(new UnwritableVectorIJK(-0.17677670, -0.91855865, 0.35355339), ithColumn);
    assertEqualVector(new UnwritableVectorIJK(0.88388348, -0.30618622, -0.35355339), jthColumn);
    assertEqualVector(new UnwritableVectorIJK(0.43301270, 0.25000000, 0.86602540), kthColumn);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testUnwritableRotationMatrixIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKException() {
    new UnwritableRotationMatrixIJK(1.0, scaledIthColumn, 1.0, scaledJthColumn, 1.0,
        scaledKthColumn);
  }

  @Test
  public void testUnwritableRotationMatrixIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJK() {
    assertEquivalentMatrix(new UnwritableRotationMatrixIJK(-0.17677670, -0.91855865, 0.35355339,
        0.88388348, -0.30618622, -0.35355339, 0.43301270, 0.25000000, 0.86602540), scaleVectorCon);
    assertEqualVector(new UnwritableVectorIJK(11.0,
        new UnwritableVectorIJK(-0.17677670, -0.91855865, 0.35355339)), scaledIthColumn);
    assertEqualVector(
        new UnwritableVectorIJK(-5, new UnwritableVectorIJK(0.88388348, -0.30618622, -0.35355339)),
        scaledJthColumn);
    assertEqualVector(
        new UnwritableVectorIJK(3, new UnwritableVectorIJK(0.43301270, 0.25000000, 0.86602540)),
        scaledKthColumn);
  }

  @Test
  public void testCreateSharpened() {
    UnwritableRotationMatrixIJK sharpened = vectorCon.createSharpened();
    assertEqualMatrix(new UnwritableRotationMatrixIJK(-0.17677670, -0.91855865, 0.35355339,
        0.88388348, -0.30618622, -0.35355339, 0.43301270, 0.25000000, 0.86602540), vectorCon);

    UnwritableVectorIJK i = sharpened.getIthColumn(new VectorIJK());
    UnwritableVectorIJK j = sharpened.getJthColumn(new VectorIJK());
    UnwritableVectorIJK k = sharpened.getKthColumn(new VectorIJK());

    AssertTools.assertEquivalentDouble(1.0, i.getLength());
    AssertTools.assertEquivalentDouble(1.0, j.getLength());
    AssertTools.assertEquivalentDouble(1.0, k.getLength());

    assertEquals(0.0, i.getDot(j), TIGHT_TOLERANCE);
    assertEquals(0.0, i.getDot(j), TIGHT_TOLERANCE);
    assertEquals(0.0, k.getDot(j), TIGHT_TOLERANCE);

  }

  @Test
  public void testCreateTranspose() {
    UnwritableRotationMatrixIJK transpose = valueCon.createTranspose();
    assertNotSame(transpose, valueCon);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
        -ROOT2RECIP, 0, ROOT2RECIP), valueCon);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, -ROOT2RECIP, 0, 1, 0,
        ROOT2RECIP, 0, ROOT2RECIP), transpose);
  }

  @Test
  public void testCreateInverse() {
    UnwritableRotationMatrixIJK transpose = valueCon.createInverse();
    assertNotSame(transpose, valueCon);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
        -ROOT2RECIP, 0, ROOT2RECIP), valueCon);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, -ROOT2RECIP, 0, 1, 0,
        ROOT2RECIP, 0, ROOT2RECIP), transpose);
    RotationMatrixIJK result = RotationMatrixIJK.mxm(valueCon, transpose, new RotationMatrixIJK());
    assertComponentEquals(RotationMatrixIJK.IDENTITY, result, LOOSE_TOLERANCE);
  }

  @Test
  public void testCreateInverseDouble() {
    UnwritableRotationMatrixIJK transpose = valueCon.createInverse(0.000001);
    assertNotSame(transpose, valueCon);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
        -ROOT2RECIP, 0, ROOT2RECIP), valueCon);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, -ROOT2RECIP, 0, 1, 0,
        ROOT2RECIP, 0, ROOT2RECIP), transpose);
    RotationMatrixIJK result = RotationMatrixIJK.mxm(valueCon, transpose, new RotationMatrixIJK());
    assertComponentEquals(RotationMatrixIJK.IDENTITY, result, LOOSE_TOLERANCE);
  }

  @Test
  public void testCopyOf() {

    UnwritableRotationMatrixIJK unwritable =
        new UnwritableRotationMatrixIJK(RotationMatrixIJK.IDENTITY);
    UnwritableRotationMatrixIJK notUnwritable =
        new UnwritableRotationMatrixIJK(RotationMatrixIJK.IDENTITY) {};

    UnwritableRotationMatrixIJK result = UnwritableRotationMatrixIJK.copyOf(unwritable);
    assertSame(result, unwritable);

    result = UnwritableRotationMatrixIJK.copyOf(notUnwritable);
    assertNotSame(result, notUnwritable);
    assertEquals(result, notUnwritable);
    assertEquals(UnwritableRotationMatrixIJK.class, result.getClass());

  }

}
