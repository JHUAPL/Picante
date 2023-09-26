package picante.math.vectorspace;

import static picante.junit.AssertTools.assertEqualDouble;
import org.junit.Before;
import org.junit.Test;

public class InternalOperationsTest {

  private static final double LOOSE_TOLERANCE = 1e-14;

  private UnwritableRotationMatrixIJK m;
  private UnwritableMatrixIJ m2;

  @Before
  public void setUp() {
    m = new UnwritableRotationMatrixIJK(-0.23795296, -0.95387879, 0.18301270, 0.94505974,
        -0.27086608, -0.18301270, 0.22414387, 0.12940952, 0.96592583).createSharpened();
    double theta = 12.89345 * Math.PI / 180.0;
    m2 = new UnwritableMatrixIJ(Math.cos(theta), Math.sin(theta), -Math.sin(theta),
        Math.cos(theta));
  }

  @Test
  public void testAbsMaxComponent() {
    assertEqualDouble(10, InternalOperations.absMaxComponent(1, 2, 10));
    assertEqualDouble(10, InternalOperations.absMaxComponent(1, 2, -10));
    assertEqualDouble(10, InternalOperations.absMaxComponent(1, 10, 2));
    assertEqualDouble(10, InternalOperations.absMaxComponent(1, -10, 2));
    assertEqualDouble(10, InternalOperations.absMaxComponent(10, 1, 2));
    assertEqualDouble(10, InternalOperations.absMaxComponent(-10, 1, 2));
    assertEqualDouble(10, InternalOperations.absMaxComponent(-10, 10, -10));
    assertEqualDouble(10, InternalOperations.absMaxComponent(10 - Math.ulp(10), 1, -10));
  }

  @Test
  public void testComputeNorm() {
    assertEqualDouble(2, InternalOperations.computeNorm(2, 0, 0));
    assertEqualDouble(2, InternalOperations.computeNorm(0, 2, 0));
    assertEqualDouble(2, InternalOperations.computeNorm(0, 0, 2));

    /*
     * 1, 100 are so insignificant as compared to Double.MAX_VALUE the result should remain
     * unchanged.
     */
    assertEqualDouble(Double.MAX_VALUE, InternalOperations.computeNorm(Double.MAX_VALUE, 1, 100));
  }

  @Test(expected = MalformedRotationException.class)
  public void testCheckRotationIthColumnException() throws MalformedRotationException {
    UnwritableMatrixIJK nm = new UnwritableMatrixIJK(1.00001, 1.0, 1.0, m);
    InternalOperations.checkRotation(nm.ii, nm.ji, nm.ki, nm.ij, nm.jj, nm.kj, nm.ik, nm.jk, nm.kk,
        LOOSE_TOLERANCE, LOOSE_TOLERANCE);
  }

  @Test(expected = MalformedRotationException.class)
  public void testCheckRotationJthColumnException() throws MalformedRotationException {
    UnwritableMatrixIJK nm = new UnwritableMatrixIJK(1.0, 1.00001, 1.0, m);
    InternalOperations.checkRotation(nm.ii, nm.ji, nm.ki, nm.ij, nm.jj, nm.kj, nm.ik, nm.jk, nm.kk,
        LOOSE_TOLERANCE, LOOSE_TOLERANCE);
  }

  @Test(expected = MalformedRotationException.class)
  public void testCheckRotationKthColumnException() throws MalformedRotationException {
    UnwritableMatrixIJK nm = new UnwritableMatrixIJK(1.0, 1.0, 1.00001, m);
    InternalOperations.checkRotation(nm.ii, nm.ji, nm.ki, nm.ij, nm.jj, nm.kj, nm.ik, nm.jk, nm.kk,
        LOOSE_TOLERANCE, LOOSE_TOLERANCE);
  }

  @Test(expected = MalformedRotationException.class)
  public void testCheckRotationDeterminantException() throws MalformedRotationException {
    VectorIJK i = m.getIthColumn(new VectorIJK());
    VectorIJK j = m.getJthColumn(new VectorIJK());
    j = VectorIJK.add(i, j, j);
    UnwritableMatrixIJK nm = new UnwritableMatrixIJK(i, j, m.getKthColumn(new VectorIJK()));
    InternalOperations.checkRotation(nm.ii, nm.ji, nm.ki, nm.ij, nm.jj, nm.kj, nm.ik, nm.jk, nm.kk,
        LOOSE_TOLERANCE, LOOSE_TOLERANCE);
  }

  @Test
  public void testCheckRotation() throws MalformedRotationException {
    InternalOperations.checkRotation(m.ii, m.ji, m.ki, m.ij, m.jj, m.kj, m.ik, m.jk, m.kk,
        LOOSE_TOLERANCE, LOOSE_TOLERANCE);
  }

  @Test
  public void testComputeDeterminant() {
    assertEqualDouble(0.0, InternalOperations.computeDeterminant(1, 2, 3, 4, 5, 6, 7, 8, 9));
    assertEqualDouble(1.0, InternalOperations.computeDeterminant(1, 0, 0, 0, 1, 0, 0, 0, 1));
    assertEqualDouble(24.0, InternalOperations.computeDeterminant(1, 5, 13, 2, 7, 17, 3, 11, 19));
  }

  @Test
  public void testAbsMaxComponentIJ() {
    assertEqualDouble(10, InternalOperations.absMaxComponent(2, 10));
    assertEqualDouble(10, InternalOperations.absMaxComponent(2, -10));
    assertEqualDouble(10, InternalOperations.absMaxComponent(10, 2));
    assertEqualDouble(10, InternalOperations.absMaxComponent(-10, 2));
    assertEqualDouble(10, InternalOperations.absMaxComponent(-10, 10));
    assertEqualDouble(10, InternalOperations.absMaxComponent(10 - Math.ulp(10), -10));
  }

  @Test
  public void testComputeNormIJ() {
    assertEqualDouble(2, InternalOperations.computeNorm(2, 0));
    assertEqualDouble(2, InternalOperations.computeNorm(0, 2));

    /*
     * 1, 100 are so insignificant as compared to Double.MAX_VALUE the result should remain
     * unchanged.
     */
    assertEqualDouble(Double.MAX_VALUE, InternalOperations.computeNorm(Double.MAX_VALUE, 100));
  }

  @Test(expected = MalformedRotationException.class)
  public void testCheckRotationIthColumnExceptionIJ() throws MalformedRotationException {
    UnwritableMatrixIJ nm = new UnwritableMatrixIJ(1.00001, 1.0, m2);
    InternalOperations.checkRotation(nm.ii, nm.ji, nm.ij, nm.jj, LOOSE_TOLERANCE, LOOSE_TOLERANCE);
  }

  @Test(expected = MalformedRotationException.class)
  public void testCheckRotationJthColumnExceptionIJ() throws MalformedRotationException {
    UnwritableMatrixIJ nm = new UnwritableMatrixIJ(1.0, 1.00001, m2);
    InternalOperations.checkRotation(nm.ii, nm.ji, nm.ij, nm.jj, LOOSE_TOLERANCE, LOOSE_TOLERANCE);
  }

  @Test(expected = MalformedRotationException.class)
  public void testCheckRotationDeterminantExceptionIJ() throws MalformedRotationException {
    VectorIJ i = m2.getIthColumn(new VectorIJ());
    VectorIJ j = m2.getJthColumn(new VectorIJ());
    j = VectorIJ.add(i, j, j);
    UnwritableMatrixIJ nm = new UnwritableMatrixIJ(i, j);
    InternalOperations.checkRotation(nm.ii, nm.ji, nm.ij, nm.jj, LOOSE_TOLERANCE, LOOSE_TOLERANCE);
  }

  @Test
  public void testCheckRotationIJ() throws MalformedRotationException {
    InternalOperations.checkRotation(m2.ii, m2.ji, m2.ij, m2.jj, LOOSE_TOLERANCE, LOOSE_TOLERANCE);
  }

  @Test
  public void testComputeDeterminantIJ() {
    assertEqualDouble(-2.0, InternalOperations.computeDeterminant(1, 2, 3, 4));
    assertEqualDouble(1.0, InternalOperations.computeDeterminant(1, 0, 0, 1));
    assertEqualDouble(-63.0, InternalOperations.computeDeterminant(1, 5, 13, 2));
  }
}
