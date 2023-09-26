package picante.math.vectorspace;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
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
 * Note the constructor based tests presented here are largely just copies of the methods from the
 * Unwritable parent class. This is acceptable as we are only just kicking the tires.
 */
public class RotationMatrixIJKTest {

  private static final double LOOSE_TOLERANCE = 1.0E-14;
  private static final double TIGHT_TOLERANCE = 1.5E-16;

  private static final double ROOT3BY2 = Math.sqrt(3) / 2.0;
  private static final double ROOT2RECIP = 1.0 / Math.sqrt(2);

  private RotationMatrixIJK defaultCon;
  private RotationMatrixIJK valueCon;
  private RotationMatrixIJK arrayCon;
  private RotationMatrixIJK copyCon;
  private RotationMatrixIJK copyRotCon;
  private RotationMatrixIJK scaleCon;
  private RotationMatrixIJK scaleColCon;
  private RotationMatrixIJK vectorCon;
  private RotationMatrixIJK scaleVectorCon;

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

  private RotationMatrixIJK r;
  private RotationMatrixIJK s;
  private RotationMatrixIJK t;

  private MatrixIJK o;

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

    defaultCon = new RotationMatrixIJK();
    valueCon =
        new RotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0, -ROOT2RECIP, 0, ROOT2RECIP);
    arrayCon = new RotationMatrixIJK(array);
    copyCon = new RotationMatrixIJK(matrix);
    copyRotCon = new RotationMatrixIJK(valueCon);
    scaleCon = new RotationMatrixIJK(1.0 / 5.0, scaledMatrix);
    scaleColCon = new RotationMatrixIJK(0.5, -1.0 / 3.0, 1.0 / 4.0, scaledColMatrix);
    vectorCon = new RotationMatrixIJK(ithColumn, jthColumn, kthColumn);
    scaleVectorCon = new RotationMatrixIJK(1.0 / 11.0, scaledIthColumn, -1.0 / 5.0, scaledJthColumn,
        1.0 / 3.0, scaledKthColumn);

    r = new RotationMatrixIJK(-0.17677670, -0.91855865, 0.35355339, 0.88388348, -0.30618622,
        -0.35355339, 0.43301270, 0.25000000, 0.86602540);
    s = new RotationMatrixIJK(-0.23795296, -0.95387879, 0.18301270, 0.94505974, -0.27086608,
        -0.18301270, 0.22414387, 0.12940952, 0.96592583).sharpen();
    t = new RotationMatrixIJK();

    o = new MatrixIJK();

  }

  @After
  public void tearDown() throws Exception {
    checkStaticFinalMembers();
  }

  @Test(expected = MalformedRotationException.class)
  public void testCheckRotationIthColumnException() throws MalformedRotationException {
    UnwritableMatrixIJK nm = new UnwritableMatrixIJK(1.01, 1.0, 1.0, matrix);
    UnwritableRotationMatrixIJK.checkRotation(nm);
  }

  @Test(expected = MalformedRotationException.class)
  public void testCheckRotationJthColumnException() throws MalformedRotationException {
    UnwritableMatrixIJK nm = new UnwritableMatrixIJK(1.0, 1.01, 1.0, matrix);
    UnwritableRotationMatrixIJK.checkRotation(nm);
  }

  @Test(expected = MalformedRotationException.class)
  public void testCheckRotationKthColumnException() throws MalformedRotationException {
    UnwritableMatrixIJK nm = new UnwritableMatrixIJK(1.0, 1.0, 1.01, matrix);
    UnwritableRotationMatrixIJK.checkRotation(nm);
  }

  @Test(expected = MalformedRotationException.class)
  public void testCheckRotationDeterminantException() throws MalformedRotationException {
    VectorIJK i = matrix.getIthColumn(new VectorIJK());
    VectorIJK j = matrix.getJthColumn(new VectorIJK());
    j = VectorIJK.add(i, j, j);
    UnwritableMatrixIJK nm = new UnwritableMatrixIJK(i, j, matrix.getKthColumn(new VectorIJK()));
    UnwritableRotationMatrixIJK.checkRotation(nm);
  }

  @Test
  public void testCheckRotation() throws MalformedRotationException {
    UnwritableRotationMatrixIJK.checkRotation(matrix);
  }

  @Test
  public void testRotationMatrixIJK() {
    assertEqualMatrix(RotationMatrixIJK.IDENTITY, defaultCon);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testRotationMatrixIJKDoubleDoubleDoubleDoubleDoubleDoubleDoubleDoubleDoubleException() {
    new RotationMatrixIJK(scaledColMatrix.ii, scaledColMatrix.ji, scaledColMatrix.ki,
        scaledColMatrix.ij, scaledColMatrix.jj, scaledColMatrix.kj, scaledColMatrix.ik,
        scaledColMatrix.jk, scaledColMatrix.kk);
  }

  @Test
  public void testRotationMatrixIJKDoubleDoubleDoubleDoubleDoubleDoubleDoubleDoubleDouble() {
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
  public void testRotationMatrixIJKDoubleArrayArrayIndexException() {
    double[][] badArray = {{1, 0, 0}, {0, 0, 1}};
    new RotationMatrixIJK(badArray);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testRotationMatrixIJKDoubleArrayArrayException() {
    double[][] badArray = {{scaledColMatrix.ii, scaledColMatrix.ij, scaledColMatrix.ik},
        {scaledColMatrix.ji, scaledColMatrix.jj, scaledColMatrix.jk},
        {scaledColMatrix.ki, scaledColMatrix.kj, scaledColMatrix.kk}};
    new RotationMatrixIJK(badArray);
  }

  @Test
  public void testRotationMatrixIJKDoubleArrayArray() {
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
  public void testRotationMatrixIJKUnwritableRotationMatrixIJK() {
    assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
        -ROOT2RECIP, 0, ROOT2RECIP), copyRotCon);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
        -ROOT2RECIP, 0, ROOT2RECIP), valueCon);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testRotationMatrixIJKUnwritableMatrixIJKException() {
    new RotationMatrixIJK(scaledColMatrix);
  }

  @Test
  public void testRotationMatrixIJKUnwritableMatrixIJK() {
    assertEqualMatrix(new UnwritableRotationMatrixIJK(-0.23795296, -0.95387879, 0.18301270,
        0.94505974, -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583), copyCon);
    assertEqualMatrix(new UnwritableMatrixIJK(-0.23795296, -0.95387879, 0.18301270, 0.94505974,
        -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583), matrix);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testRotationMatrixIJKDoubleUnwritableMatrixIJKException() {
    new RotationMatrixIJK(10.0, scaledColMatrix);
  }

  @Test
  public void testRotationMatrixIJKDoubleUnwritableMatrixIJK() {
    assertEquivalentMatrix(new UnwritableRotationMatrixIJK(-0.23795296, -0.95387879, 0.18301270,
        0.94505974, -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583), scaleCon);
    assertEqualMatrix(
        new UnwritableMatrixIJK(5.0, new UnwritableMatrixIJK(-0.23795296, -0.95387879, 0.18301270,
            0.94505974, -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583)),
        scaledMatrix);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testRotationMatrixIJKDoubleDoubleDoubleUnwritableMatrixIJKException() {
    new RotationMatrixIJK(10.0, 12.0, 13.0, matrix);
  }

  @Test
  public void testRotationMatrixIJKDoubleDoubleDoubleUnwritableMatrixIJK() {
    assertEquivalentMatrix(new UnwritableRotationMatrixIJK(-0.23795296, -0.95387879, 0.18301270,
        0.94505974, -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583), scaleColCon);
    assertEqualMatrix(
        new UnwritableMatrixIJK(2.0, -3.0, 4.0, new UnwritableMatrixIJK(-0.23795296, -0.95387879,
            0.18301270, 0.94505974, -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583)),
        scaledColMatrix);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testRotationMatrixIJKUnwritableVectorIJKUnwritableVectorIJKUnwritableVectorIJKException() {
    new RotationMatrixIJK(scaledIthColumn, scaledJthColumn, scaledKthColumn);
  }

  @Test
  public void testRotationMatrixIJKUnwritableVectorIJKUnwritableVectorIJKUnwritableVectorIJK() {
    assertEqualMatrix(new UnwritableRotationMatrixIJK(-0.17677670, -0.91855865, 0.35355339,
        0.88388348, -0.30618622, -0.35355339, 0.43301270, 0.25000000, 0.86602540), vectorCon);
    assertEqualVector(new UnwritableVectorIJK(-0.17677670, -0.91855865, 0.35355339), ithColumn);
    assertEqualVector(new UnwritableVectorIJK(0.88388348, -0.30618622, -0.35355339), jthColumn);
    assertEqualVector(new UnwritableVectorIJK(0.43301270, 0.25000000, 0.86602540), kthColumn);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testRotationMatrixIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKException() {
    new RotationMatrixIJK(1.0, scaledIthColumn, 1.0, scaledJthColumn, 1.0, scaledKthColumn);
  }

  @Test
  public void testRotationMatrixIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJK() {
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
    RotationMatrixIJK sharpened = vectorCon.createSharpened();
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
    RotationMatrixIJK transpose = valueCon.createTranspose();
    assertNotSame(transpose, valueCon);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
        -ROOT2RECIP, 0, ROOT2RECIP), valueCon);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, -ROOT2RECIP, 0, 1, 0,
        ROOT2RECIP, 0, ROOT2RECIP), transpose);

  }

  @Test
  public void testCreateInverse() {
    RotationMatrixIJK transpose = valueCon.createInverse();
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
    RotationMatrixIJK transpose = valueCon.createInverse(0.000001);
    assertNotSame(transpose, valueCon);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
        -ROOT2RECIP, 0, ROOT2RECIP), valueCon);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, -ROOT2RECIP, 0, 1, 0,
        ROOT2RECIP, 0, ROOT2RECIP), transpose);
    RotationMatrixIJK result = RotationMatrixIJK.mxm(valueCon, transpose, new RotationMatrixIJK());
    assertComponentEquals(RotationMatrixIJK.IDENTITY, result, LOOSE_TOLERANCE);

  }

  @Test
  public void testSharpen() {
    RotationMatrixIJK sharpened = vectorCon.sharpen();
    assertSame(sharpened, vectorCon);

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
  public void testTranspose() {
    RotationMatrixIJK transpose = valueCon.transpose();
    assertSame(transpose, valueCon);
    assertEqualMatrix(
        new UnwritableMatrixIJK(ROOT2RECIP, 0, -ROOT2RECIP, 0, 1, 0, ROOT2RECIP, 0, ROOT2RECIP),
        valueCon);
  }

  @Test
  public void testSetToDoubleDoubleDoubleDoubleDoubleDoubleDoubleDoubleDoubleException() {
    try {
      valueCon.setTo(scaledColMatrix.ii, scaledColMatrix.ji, scaledColMatrix.ki, scaledColMatrix.ij,
          scaledColMatrix.jj, scaledColMatrix.kj, scaledColMatrix.ik, scaledColMatrix.jk,
          scaledColMatrix.kk);
      fail("Expected exception not generated.");
    } catch (IllegalArgumentException e) {
      assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
          -ROOT2RECIP, 0, ROOT2RECIP), valueCon);
    }
  }

  @Test
  public void testSetToDoubleDoubleDoubleDoubleDoubleDoubleDoubleDoubleDouble() {
    RotationMatrixIJK set = valueCon.setTo(-0.23795296, -0.95387879, 0.18301270, 0.94505974,
        -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583);
    assertSame(set, valueCon);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(-0.23795296, -0.95387879, 0.18301270,
        0.94505974, -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583), set);
  }

  @Test
  public void testSetToDoubleArrayArrayIndexException() {
    double[][] badArray = {{1, 0, 0}, {0, 0, 1}};
    try {
      valueCon.setTo(badArray);
      fail("Expected exception not generated.");
    } catch (IndexOutOfBoundsException e) {
      assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
          -ROOT2RECIP, 0, ROOT2RECIP), valueCon);
      AssertTools.assertEqualDouble(1, badArray[0][0]);
      AssertTools.assertEqualDouble(0, badArray[0][1]);
      AssertTools.assertEqualDouble(0, badArray[0][2]);
      AssertTools.assertEqualDouble(0, badArray[1][0]);
      AssertTools.assertEqualDouble(0, badArray[1][1]);
      AssertTools.assertEqualDouble(1, badArray[1][2]);
    }
  }

  @Test
  public void testSetToDoubleArrayArrayException() {
    double[][] badArray = {{scaledColMatrix.ii, scaledColMatrix.ij, scaledColMatrix.ik},
        {scaledColMatrix.ji, scaledColMatrix.jj, scaledColMatrix.jk},
        {scaledColMatrix.ki, scaledColMatrix.kj, scaledColMatrix.kk}};
    try {
      valueCon.setTo(badArray);
      fail("Expected exception not generated.");
    } catch (IllegalArgumentException e) {
      assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
          -ROOT2RECIP, 0, ROOT2RECIP), valueCon);
      AssertTools.assertEqualDouble(scaledColMatrix.ii, badArray[0][0]);
      AssertTools.assertEqualDouble(scaledColMatrix.ij, badArray[0][1]);
      AssertTools.assertEqualDouble(scaledColMatrix.ik, badArray[0][2]);
      AssertTools.assertEqualDouble(scaledColMatrix.ji, badArray[1][0]);
      AssertTools.assertEqualDouble(scaledColMatrix.jj, badArray[1][1]);
      AssertTools.assertEqualDouble(scaledColMatrix.jk, badArray[1][2]);
      AssertTools.assertEqualDouble(scaledColMatrix.ki, badArray[2][0]);
      AssertTools.assertEqualDouble(scaledColMatrix.kj, badArray[2][1]);
      AssertTools.assertEqualDouble(scaledColMatrix.kk, badArray[2][2]);
    }
  }

  @Test
  public void testSetToDoubleArrayArray() {
    RotationMatrixIJK set = valueCon.setTo(array);
    assertSame(set, valueCon);

    assertEqualMatrix(new UnwritableRotationMatrixIJK(0.5, -ROOT3BY2, 0, ROOT3BY2, 0.5, 0, 0, 0, 1),
        valueCon);
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
  public void testSetToUnwritableVectorIJKUnwritableVectorIJKUnwritableVectorIJKException() {
    try {
      valueCon.setTo(scaledIthColumn, scaledJthColumn, scaledKthColumn);
      fail("Expected exception not generated.");
    } catch (IllegalArgumentException e) {
      assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
          -ROOT2RECIP, 0, ROOT2RECIP), valueCon);
      assertEqualVector(new UnwritableVectorIJK(11.0,
          new UnwritableVectorIJK(-0.17677670, -0.91855865, 0.35355339)), scaledIthColumn);
      assertEqualVector(new UnwritableVectorIJK(-5,
          new UnwritableVectorIJK(0.88388348, -0.30618622, -0.35355339)), scaledJthColumn);
      assertEqualVector(
          new UnwritableVectorIJK(3, new UnwritableVectorIJK(0.43301270, 0.25000000, 0.86602540)),
          scaledKthColumn);
    }

  }

  @Test
  public void testSetToUnwritableVectorIJKUnwritableVectorIJKUnwritableVectorIJK() {
    RotationMatrixIJK set = valueCon.setTo(ithColumn, jthColumn, kthColumn);
    assertSame(set, valueCon);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(-0.17677670, -0.91855865, 0.35355339,
        0.88388348, -0.30618622, -0.35355339, 0.43301270, 0.25000000, 0.86602540), set);
    assertEqualVector(new UnwritableVectorIJK(-0.17677670, -0.91855865, 0.35355339), ithColumn);
    assertEqualVector(new UnwritableVectorIJK(0.88388348, -0.30618622, -0.35355339), jthColumn);
    assertEqualVector(new UnwritableVectorIJK(0.43301270, 0.25000000, 0.86602540), kthColumn);
  }

  @Test
  public void testSetToDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKException() {
    try {
      valueCon.setTo(1.0, scaledIthColumn, 1.0, scaledJthColumn, 1.0, scaledKthColumn);
      fail("Expected exception not generated.");
    } catch (IllegalArgumentException e) {
      assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
          -ROOT2RECIP, 0, ROOT2RECIP), valueCon);
      assertEqualVector(new UnwritableVectorIJK(11.0,
          new UnwritableVectorIJK(-0.17677670, -0.91855865, 0.35355339)), scaledIthColumn);
      assertEqualVector(new UnwritableVectorIJK(-5,
          new UnwritableVectorIJK(0.88388348, -0.30618622, -0.35355339)), scaledJthColumn);
      assertEqualVector(
          new UnwritableVectorIJK(3, new UnwritableVectorIJK(0.43301270, 0.25000000, 0.86602540)),
          scaledKthColumn);
    }
  }

  @Test
  public void testSetToDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJK() {
    RotationMatrixIJK set = valueCon.setTo(1.0 / 11.0, scaledIthColumn, -1.0 / 5.0, scaledJthColumn,
        1.0 / 3.0, scaledKthColumn);
    assertEquivalentMatrix(new UnwritableRotationMatrixIJK(-0.17677670, -0.91855865, 0.35355339,
        0.88388348, -0.30618622, -0.35355339, 0.43301270, 0.25000000, 0.86602540), set);
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
  public void testSetToUnwritableRotationMatrixIJK() {
    RotationMatrixIJK set = valueCon.setTo(vectorCon);
    assertSame(set, valueCon);
    assertNotSame(set, vectorCon);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(-0.17677670, -0.91855865, 0.35355339,
        0.88388348, -0.30618622, -0.35355339, 0.43301270, 0.25000000, 0.86602540), set);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(-0.17677670, -0.91855865, 0.35355339,
        0.88388348, -0.30618622, -0.35355339, 0.43301270, 0.25000000, 0.86602540), vectorCon);
  }

  @Test
  public void testSetToSharpenedUnwritableRotationMatrixIJK() {
    RotationMatrixIJK sharpened = valueCon.setToSharpened(vectorCon);
    assertSame(sharpened, valueCon);

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
  public void testSetToTransposeUnwritableRotationMatrixIJK() {
    RotationMatrixIJK set = valueCon.setToTranspose(vectorCon);
    assertSame(set, valueCon);
    assertNotSame(vectorCon, valueCon);

    assertEqualMatrix(
        new UnwritableRotationMatrixIJK(-0.17677670, -0.91855865, 0.35355339, 0.88388348,
            -0.30618622, -0.35355339, 0.43301270, 0.25000000, 0.86602540).createTranspose(),
        valueCon);
    assertEqualMatrix(new UnwritableRotationMatrixIJK(-0.17677670, -0.91855865, 0.35355339,
        0.88388348, -0.30618622, -0.35355339, 0.43301270, 0.25000000, 0.86602540), vectorCon);
  }

  @Test
  public void testSetToUnwritableMatrixIJKException() {
    try {
      valueCon.setTo(scaledMatrix);
      fail("Expected exception not generated.");
    } catch (IllegalArgumentException e) {
      assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
          -ROOT2RECIP, 0, ROOT2RECIP), valueCon);
      assertEqualMatrix(
          new UnwritableMatrixIJK(5.0, new UnwritableMatrixIJK(-0.23795296, -0.95387879, 0.18301270,
              0.94505974, -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583)),
          scaledMatrix);
    }
  }

  @Test
  public void testSetToUnwritableMatrixIJK() {
    RotationMatrixIJK set = valueCon.setTo(matrix);
    assertSame(set, valueCon);
    assertEqualMatrix(new UnwritableMatrixIJK(-0.23795296, -0.95387879, 0.18301270, 0.94505974,
        -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583), set);
    assertEqualMatrix(new UnwritableMatrixIJK(-0.23795296, -0.95387879, 0.18301270, 0.94505974,
        -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583), matrix);
  }

  @Test
  public void testSetToSharpenedUnwritableMatrixIJKException() {
    try {
      valueCon.setToSharpened(scaledMatrix);
      fail("Expected expection not generated.");
    } catch (IllegalArgumentException e) {
      assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
          -ROOT2RECIP, 0, ROOT2RECIP), valueCon);
      assertEqualMatrix(
          new UnwritableMatrixIJK(5.0, new UnwritableMatrixIJK(-0.23795296, -0.95387879, 0.18301270,
              0.94505974, -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583)),
          scaledMatrix);
    }
  }

  @Test
  public void testSetToSharpenedUnwritableMatrixIJK() {
    RotationMatrixIJK set = valueCon.setToSharpened(matrix);
    assertSame(set, valueCon);

    assertEqualMatrix(new UnwritableMatrixIJK(-0.23795296, -0.95387879, 0.18301270, 0.94505974,
        -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583), matrix);

    UnwritableVectorIJK i = set.getIthColumn(new VectorIJK());
    UnwritableVectorIJK j = set.getJthColumn(new VectorIJK());
    UnwritableVectorIJK k = set.getKthColumn(new VectorIJK());

    AssertTools.assertEquivalentDouble(1.0, i.getLength());
    AssertTools.assertEquivalentDouble(1.0, j.getLength());
    AssertTools.assertEquivalentDouble(1.0, k.getLength());

    assertEquals(0.0, i.getDot(j), TIGHT_TOLERANCE);
    assertEquals(0.0, i.getDot(j), TIGHT_TOLERANCE);
    assertEquals(0.0, k.getDot(j), TIGHT_TOLERANCE);
  }

  @Test
  public void testSetToTransposeUnwritableMatrixIJKException() {
    try {
      valueCon.setToTranspose(scaledMatrix);
      fail("Expected expection not generated.");
    } catch (IllegalArgumentException e) {
      assertEqualMatrix(new UnwritableRotationMatrixIJK(ROOT2RECIP, 0, ROOT2RECIP, 0, 1, 0,
          -ROOT2RECIP, 0, ROOT2RECIP), valueCon);
      assertEqualMatrix(
          new UnwritableMatrixIJK(5.0, new UnwritableMatrixIJK(-0.23795296, -0.95387879, 0.18301270,
              0.94505974, -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583)),
          scaledMatrix);
    }
  }

  @Test
  public void testSetToTransposeUnwritableMatrixIJK() {
    RotationMatrixIJK set = valueCon.setToTranspose(matrix);
    assertSame(set, valueCon);

    assertEqualMatrix(new UnwritableMatrixIJK(-0.23795296, -0.95387879, 0.18301270, 0.94505974,
        -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583).createTranspose(), set);
    assertEqualMatrix(new UnwritableMatrixIJK(-0.23795296, -0.95387879, 0.18301270, 0.94505974,
        -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583), matrix);
  }

  @Test
  public void testMxmUnwritableRotationMatrixIJKUnwritableRotationMatrixIJKRotationMatrixIJKOverR() {
    MatrixIJK expected = MatrixIJK.mxm(r, s, o);
    RotationMatrixIJK m = RotationMatrixIJK.mxm(r, s, r);
    assertSame(m, r);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testMxmUnwritableRotationMatrixIJKUnwritableRotationMatrixIJKRotationMatrixIJKOverS() {
    MatrixIJK expected = MatrixIJK.mxm(r, s, o);
    RotationMatrixIJK m = RotationMatrixIJK.mxm(r, s, s);
    assertSame(m, s);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testMxmUnwritableRotationMatrixIJKUnwritableRotationMatrixIJKRotationMatrixIJKRROverR() {
    MatrixIJK expected = MatrixIJK.mxm(r, r, o);
    RotationMatrixIJK m = RotationMatrixIJK.mxm(r, r, r);
    assertSame(m, r);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testMxmUnwritableRotationMatrixIJKUnwritableRotationMatrixIJKRotationMatrixIJK() {
    MatrixIJK expected = MatrixIJK.mxm(r, s, o);
    RotationMatrixIJK m = RotationMatrixIJK.mxm(r, s, t);
    assertSame(m, t);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testNewMxmUnwritableRotationMatrixIJKUnwritableRotationMatrixIJKRotationMatrixIJK() {
    MatrixIJK expected = MatrixIJK.mxm(r, s, o);
    RotationMatrixIJK m = RotationMatrixIJK.mxm(r, s);
    assertNotSame(m, s);
    assertNotSame(m, r);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testMtxmUnwritableRotationMatrixIJKUnwritableRotationMatrixIJKRotationMatrixIJKOverR() {
    MatrixIJK expected = MatrixIJK.mtxm(r, s, o);
    RotationMatrixIJK m = RotationMatrixIJK.mtxm(r, s, r);
    assertSame(m, r);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testMtxmUnwritableRotationMatrixIJKUnwritableRotationMatrixIJKRotationMatrixIJKOverS() {
    MatrixIJK expected = MatrixIJK.mtxm(r, s, o);
    RotationMatrixIJK m = RotationMatrixIJK.mtxm(r, s, s);
    assertSame(m, s);
    assertEqualMatrix(expected, m);
  }

  public void testMtxmUnwritableRotationMatrixIJKUnwritableRotationMatrixIJKRotationMatrixIJKRROverR() {
    MatrixIJK expected = MatrixIJK.mtxm(r, r, o);
    RotationMatrixIJK m = RotationMatrixIJK.mtxm(r, r, r);
    assertSame(m, r);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testMtxmUnwritableRotationMatrixIJKUnwritableRotationMatrixIJKRotationMatrixIJK() {
    MatrixIJK expected = MatrixIJK.mtxm(r, s, o);
    RotationMatrixIJK m = RotationMatrixIJK.mtxm(r, s, t);
    assertSame(m, t);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testNewMtxmUnwritableRotationMatrixIJKUnwritableRotationMatrixIJKRotationMatrixIJK() {
    MatrixIJK expected = MatrixIJK.mtxm(r, s, o);
    RotationMatrixIJK m = RotationMatrixIJK.mtxm(r, s);
    assertNotSame(m, s);
    assertNotSame(m, r);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testMxmtUnwritableRotationMatrixIJKUnwritableRotationMatrixIJKRotationMatrixIJKOverR() {
    MatrixIJK expected = MatrixIJK.mxmt(r, s, o);
    RotationMatrixIJK m = RotationMatrixIJK.mxmt(r, s, r);
    assertSame(m, r);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testMxmtUnwritableRotationMatrixIJKUnwritableRotationMatrixIJKRotationMatrixIJKOverS() {
    MatrixIJK expected = MatrixIJK.mxmt(r, s, o);
    RotationMatrixIJK m = RotationMatrixIJK.mxmt(r, s, s);
    assertSame(m, s);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testMxmtUnwritableRotationMatrixIJKUnwritableRotationMatrixIJKRotationMatrixIJKRROverR() {
    MatrixIJK expected = MatrixIJK.mxmt(r, r, o);
    RotationMatrixIJK m = RotationMatrixIJK.mxmt(r, r, r);
    assertSame(m, r);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testMxmtUnwritableRotationMatrixIJKUnwritableRotationMatrixIJKRotationMatrixIJK() {
    MatrixIJK expected = MatrixIJK.mxmt(r, s, o);
    RotationMatrixIJK m = RotationMatrixIJK.mxmt(r, s, t);
    assertSame(m, t);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testNewMxmtUnwritableRotationMatrixIJKUnwritableRotationMatrixIJKRotationMatrixIJK() {
    MatrixIJK expected = MatrixIJK.mxmt(r, s, o);
    RotationMatrixIJK m = RotationMatrixIJK.mxmt(r, s);
    assertNotSame(m, s);
    assertNotSame(m, r);
    assertEqualMatrix(expected, m);
  }

  @Test
  public void testCreateSharpenedWithArguments() {
    RotationMatrixIJK actual = RotationMatrixIJK.createSharpened(0.494, -0.668, 0.557, 0.748, 0.0,
        -0.663, 0.443, 0.744, 0.5);
    RotationMatrixIJK expected = new RotationMatrixIJK(0.49387432497440653, -0.66783005887227442,
        0.55685829759260008, 0.74823778843783062, 1.4762133753517544E-004, -0.66343062196555624,
        0.44297670715821885, 0.74431377165453216, 0.49976859269132379);
    assertComponentEquals(expected, actual, 1e-15);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testCreateSharpenedWithArgumentsLeftHandedFailure() {
    RotationMatrixIJK.createSharpened(1, 0, 0, 0, 1, 0, 0, 0, -1);
  }

  static void checkStaticFinalMembers() {
    assertEqualMatrix(new UnwritableRotationMatrixIJK(1, 0, 0, 0, 1, 0, 0, 0, 1),
        RotationMatrixIJK.IDENTITY);
  }

}
