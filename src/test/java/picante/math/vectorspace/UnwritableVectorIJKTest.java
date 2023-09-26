package picante.math.vectorspace;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEqualDouble;
import static picante.junit.AssertTools.assertEqualVector;
import static picante.junit.AssertTools.assertEquivalentDouble;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import picante.math.PicanteMath;
import picante.units.FundamentalPhysicalConstants;

public class UnwritableVectorIJKTest {

  private static final double TOLERANCE = 1e-15;

  private UnwritableVectorIJK threeVarCon;
  private UnwritableVectorIJK copyCon;
  private UnwritableVectorIJK scaleCon;

  @Before
  public void setUp() throws Exception {
    threeVarCon = new UnwritableVectorIJK(1, 2, 3);
    copyCon = new UnwritableVectorIJK(threeVarCon);
    scaleCon = new UnwritableVectorIJK(-5.0, threeVarCon);
  }

  @After
  public void tearDown() throws Exception {
    VectorIJKTest.checkStaticFinalMembers();
  }

  @Test
  public void testHashCode() {

    /*
     * Simply check that vectors that are equal, but are different instances have equal hashcodes.
     * This test is not definitive, as it's possible that these two vectors
     */
    assertEquals(threeVarCon.hashCode(), copyCon.hashCode());
    assertNotSame(threeVarCon, copyCon);

  }

  @Test
  public void testUnwritableVectorIJKDoubleDoubleDouble() {
    assertEquals(1.0, threeVarCon.i, 0.0);
    assertEquals(2.0, threeVarCon.j, 0.0);
    assertEquals(3.0, threeVarCon.k, 0.0);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testUnwritableVectorIJKDoubleArrayIndexException() {
    new UnwritableVectorIJK(new double[1]);
  }

  @Test
  public void testUnwritableVectorIJKDoubleArray() {
    double[] data = {1.0, 5.5, 7.1};
    UnwritableVectorIJK dataCon = new UnwritableVectorIJK(data);
    assertEqualVector(new UnwritableVectorIJK(1, 5.5, 7.1), dataCon);
    assertEqualDouble(1.0, data[0]);
    assertEqualDouble(5.5, data[1]);
    assertEqualDouble(7.1, data[2]);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testUnwritableVectorIJKIntDoubleArrayIndexException() {
    new UnwritableVectorIJK(20, new double[10]);
  }

  @Test
  public void testUnwritableVectorIJKIntDoubleArray() {
    double[] data = {1.0, 2.0, 3.0, 5.0, 7.0, 11.0};
    UnwritableVectorIJK dataOffCon = new UnwritableVectorIJK(2, data);
    assertEqualVector(new UnwritableVectorIJK(3.0, 5.0, 7.0), dataOffCon);
    assertEqualDouble(1.0, data[0]);
    assertEqualDouble(2.0, data[1]);
    assertEqualDouble(3.0, data[2]);
    assertEqualDouble(5.0, data[3]);
    assertEqualDouble(7.0, data[4]);
    assertEqualDouble(11.0, data[5]);

  }

  @Test
  public void testUnwritableVectorIJKUnwritableVectorIJK() {
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), copyCon);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
  }

  @Test
  public void testUnwritableVectorIJKDoubleUnwritableVectorIJK() {
    assertEqualVector(new UnwritableVectorIJK(-5, -10, -15), scaleCon);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
  }

  @Test
  public void testCreateUnitized() {
    UnwritableVectorIJK unitized = threeVarCon.createUnitized();
    assertEquals(UnwritableVectorIJK.class, unitized.getClass());
    assertNotSame(unitized, threeVarCon);
    assertComponentEquals(
        new UnwritableVectorIJK(1.0 / Math.sqrt(1 + 4 + 9), new UnwritableVectorIJK(1, 2, 3)),
        unitized, 0.0);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
  }

  @Test
  public void testCreateNegated() {
    UnwritableVectorIJK negated = threeVarCon.createNegated();
    assertEquals(UnwritableVectorIJK.class, negated.getClass());
    assertNotSame(negated, threeVarCon);
    assertEqualVector(new UnwritableVectorIJK(-1.0, new UnwritableVectorIJK(1, 2, 3)), negated);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
  }

  @Test
  public void testGetI() {
    assertEqualDouble(1.0, threeVarCon.getI());
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
  }

  @Test
  public void testGetJ() {
    assertEqualDouble(2.0, threeVarCon.getJ());
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
  }

  @Test
  public void testGetK() {
    assertEqualDouble(3.0, threeVarCon.getK());
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
  }

  @Test
  public void testGetInt() {
    assertEqualDouble(1.0, threeVarCon.get(0));
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
    assertEqualDouble(2.0, threeVarCon.get(1));
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
    assertEqualDouble(3.0, threeVarCon.get(2));
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetIntLowIndexException() {
    threeVarCon.get(-1);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetIntHighIndexException() {
    threeVarCon.get(3);
  }

  @Test
  public void testGetLength() {
    assertEquivalentDouble(Math.sqrt(1 + 4 + 9), threeVarCon.getLength());
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
  }

  @Test
  public void testGetDot() {
    UnwritableVectorIJK v = new UnwritableVectorIJK(5, 7, 11);
    assertEqualDouble(5 + 14 + 33, v.getDot(threeVarCon));
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
    assertEqualVector(new UnwritableVectorIJK(5, 7, 11), v);

  }

  @Test(expected = UnsupportedOperationException.class)
  public void testGetSeparationZeroInstanceException() {
    VectorIJK.ZERO.getSeparation(VectorIJK.I);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testGetSeparationZeroArgumentException() {
    VectorIJK.I.getSeparation(VectorIJK.ZERO);
  }

  @Test
  public void testGetSeparation() {

    double TOL = 3.4E-16;

    /*
     * Test some simple vectors, where the results are obvious.
     */
    assertEqualDouble(Math.PI / 2.0, VectorIJK.I.getSeparation(VectorIJK.J));
    assertEqualDouble(Math.PI, VectorIJK.K.getSeparation(VectorIJK.MINUS_K));

    /*
     * Now exercise some more complicated examples.
     */
    UnwritableVectorIJK a = new UnwritableVectorIJK(1, 1, 1);
    UnwritableVectorIJK b = new UnwritableVectorIJK(1, 2, 3);

    assertEquals(PicanteMath.acos(6.0 / PicanteMath.sqrt(3 * (1 + 4 + 9))), a.getSeparation(b),
        TOL);
    assertEquals(Math.acos(6.0 / Math.sqrt(3 * (1 + 4 + 9))), a.getSeparation(b), TOL);
    assertEqualVector(new UnwritableVectorIJK(1, 1, 1), a);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), b);

    assertEquals(PicanteMath.acos(6.0 / PicanteMath.sqrt(3 * (1 + 4 + 9))), b.getSeparation(a),
        TOL);
    assertEquals(Math.acos(6.0 / Math.sqrt(3 * (1 + 4 + 9))), b.getSeparation(a), TOL);
    assertEqualVector(new UnwritableVectorIJK(1, 1, 1), a);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), b);

    a = new UnwritableVectorIJK(-1, -1, -1);

    assertEquivalentDouble(Math.acos(-6.0 / Math.sqrt(3 * (1 + 4 + 9))), a.getSeparation(b));
    assertEqualVector(new UnwritableVectorIJK(-1, -1, -1), a);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), b);

    assertEquivalentDouble(Math.acos(-6.0 / Math.sqrt(3 * (1 + 4 + 9))), b.getSeparation(a));
    assertEqualVector(new UnwritableVectorIJK(-1, -1, -1), a);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), b);

  }

  @Test
  public void testGetSeparationOutOfPlane() {

    /*
     * Test some simple vectors, where the results are obvious.
     */
    assertEqualDouble(0.0, VectorIJK.I.getSeparationOutOfPlane(VectorIJK.J));
    assertEqualDouble(-FundamentalPhysicalConstants.HALFPI,
        VectorIJK.K.getSeparationOutOfPlane(VectorIJK.MINUS_K));

    /*
     * Now exercise some more complicated examples.
     */
    UnwritableVectorIJK a = new UnwritableVectorIJK(1, 1, 1);
    UnwritableVectorIJK b = new UnwritableVectorIJK(1, 2, 3);

    assertEquivalentDouble(
        FundamentalPhysicalConstants.HALFPI - Math.acos(6.0 / Math.sqrt(3 * (1 + 4 + 9))),
        a.getSeparationOutOfPlane(b));
    assertEqualVector(new UnwritableVectorIJK(1, 1, 1), a);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), b);

    assertEquivalentDouble(
        FundamentalPhysicalConstants.HALFPI - Math.acos(6.0 / Math.sqrt(3 * (1 + 4 + 9))),
        b.getSeparationOutOfPlane(a));
    assertEqualVector(new UnwritableVectorIJK(1, 1, 1), a);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), b);

    a = new UnwritableVectorIJK(-1, -1, -1);

    assertEquals(FundamentalPhysicalConstants.HALFPI - Math.acos(-6.0 / Math.sqrt(3 * (1 + 4 + 9))),
        a.getSeparationOutOfPlane(b), TOLERANCE);
    assertEqualVector(new UnwritableVectorIJK(-1, -1, -1), a);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), b);

    assertEquals(FundamentalPhysicalConstants.HALFPI - Math.acos(-6.0 / Math.sqrt(3 * (1 + 4 + 9))),
        b.getSeparationOutOfPlane(a), TOLERANCE);
    assertEqualVector(new UnwritableVectorIJK(-1, -1, -1), a);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), b);

  }

  @Test
  public void testGetDistance() {
    UnwritableVectorIJK a = new UnwritableVectorIJK(1, 1, 1);
    UnwritableVectorIJK b = new UnwritableVectorIJK(1, 2, 3);

    /*
     * distance from zero
     */
    assertEquals(Math.sqrt(3), a.getDistance(VectorIJK.ZERO), TOLERANCE);
    assertEquals(Math.sqrt(1 * 1 + 2 * 2 + 3 * 3), b.getDistance(VectorIJK.ZERO), TOLERANCE);

    /*
     * distance from each other
     */
    assertEquals(2.236067977499790, a.getDistance(b), TOLERANCE);

    a = new UnwritableVectorIJK(101, 107.24, -110);
    b = new UnwritableVectorIJK(-2.5, 3, 500);
    assertEquals(6.274378276769738E2, a.getDistance(b), TOLERANCE);
  }

  @Test
  public void testMin() {
    UnwritableVectorIJK a = new UnwritableVectorIJK(1, 1, 1);
    assertEqualDouble(1, a.min());

    a = new UnwritableVectorIJK(-1, -1.1, 0);
    assertEqualDouble(-1.1, a.min());

    a = new UnwritableVectorIJK(20, 19.99, 20);
    assertEqualDouble(19.99, a.min());
  }

  @Test
  public void testMax() {
    UnwritableVectorIJK a = new UnwritableVectorIJK(1, 1, 1);
    assertEqualDouble(1, a.max());

    a = new UnwritableVectorIJK(-1, -1.1, 0);
    assertEqualDouble(0, a.max());

    a = new UnwritableVectorIJK(20, 19.99, 20);
    assertEqualDouble(20, a.max());

    a = new UnwritableVectorIJK(20.01, 19.99, 20);
    assertEqualDouble(20.01, a.max());
  }

  @Test
  public void testCopyOf() {

    UnwritableVectorIJK unwritable = new UnwritableVectorIJK(1, 2, 3);
    UnwritableVectorIJK notUnwritable = new UnwritableVectorIJK(4, 5, 6) {};

    UnwritableVectorIJK result = UnwritableVectorIJK.copyOf(unwritable);
    assertSame(result, unwritable);

    result = UnwritableVectorIJK.copyOf(notUnwritable);
    assertNotSame(result, notUnwritable);
    assertEquals(result, notUnwritable);
    assertEquals(UnwritableVectorIJK.class, result.getClass());

  }

  @Test
  public void testEqualsObject() {
    assertTrue(threeVarCon.equals(copyCon));
    assertNotSame(threeVarCon, copyCon);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), copyCon);

    assertTrue(threeVarCon.equals(threeVarCon));
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);

    assertFalse(threeVarCon.equals(null));
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);

    assertFalse(threeVarCon.equals(new String()));
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);

    assertFalse(threeVarCon.equals(scaleCon));
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
    assertEqualVector(new UnwritableVectorIJK(-5, -10, -15), scaleCon);

    UnwritableVectorIJK subClass = new UnwritableVectorIJK(1, 2, 3) {

    };
    assertTrue(threeVarCon.equals(subClass));
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), subClass);

    assertTrue(subClass.equals(threeVarCon));
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), subClass);

  }

  @Test
  public void testToString() {
    assertEquals("[1.0,2.0,3.0]", threeVarCon.toString());
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
  }

}
