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

public class UnwritableVectorIJTest {

  private UnwritableVectorIJ twoVarCon;
  private UnwritableVectorIJ copyCon;
  private UnwritableVectorIJ scaleCon;

  private static final double TIGHT_TOLERANCE = 1e-15;

  @Before
  public void setUp() throws Exception {
    twoVarCon = new UnwritableVectorIJ(1, 2);
    copyCon = new UnwritableVectorIJ(twoVarCon);
    scaleCon = new UnwritableVectorIJ(-5.0, twoVarCon);
  }

  @After
  public void tearDown() throws Exception {
    VectorIJTest.checkStaticFinalMembers();
  }

  @Test
  public void testHashCode() {

    /*
     * Simply check that vectors that are equal, but are different instances have equal hashcodes.
     * This test is not definitive, as it's possible that these two vectors
     */
    assertEquals(twoVarCon.hashCode(), copyCon.hashCode());
    assertNotSame(twoVarCon, copyCon);

  }

  @Test
  public void testUnwritableVectorIJDoubleDouble() {
    assertEquals(1.0, twoVarCon.i, 0.0);
    assertEquals(2.0, twoVarCon.j, 0.0);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testUnwritableVectorIJDoubleArrayIndexException() {
    new UnwritableVectorIJ(new double[1]);
  }

  @Test
  public void testUnwritableVectorIJDoubleArray() {
    double[] data = {1.0, 5.5};
    UnwritableVectorIJ dataCon = new UnwritableVectorIJ(data);
    assertEqualVector(new UnwritableVectorIJ(1, 5.5), dataCon);
    assertEqualDouble(1.0, data[0]);
    assertEqualDouble(5.5, data[1]);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testUnwritableVectorIJIntDoubleArrayIndexException() {
    new UnwritableVectorIJ(20, new double[10]);
  }

  @Test
  public void testUnwritableVectorIJIntDoubleArray() {
    double[] data = {1.0, 2.0, 3.0, 5.0, 7.0, 11.0};
    UnwritableVectorIJ dataOffCon = new UnwritableVectorIJ(2, data);
    assertEqualVector(new UnwritableVectorIJ(3.0, 5.0), dataOffCon);
    assertEqualDouble(1.0, data[0]);
    assertEqualDouble(2.0, data[1]);
    assertEqualDouble(3.0, data[2]);
    assertEqualDouble(5.0, data[3]);
    assertEqualDouble(7.0, data[4]);
    assertEqualDouble(11.0, data[5]);
  }

  @Test
  public void testUnwritableVectorIJUnwritableVectorIJ() {
    assertEqualVector(new UnwritableVectorIJ(1, 2), copyCon);
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
  }

  @Test
  public void testUnwritableVectorIJDoubleUnwritableVectorIJ() {
    assertEqualVector(new UnwritableVectorIJ(-5, -10), scaleCon);
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
  }

  @Test
  public void testCreateUnitized() {
    UnwritableVectorIJ unitized = twoVarCon.createUnitized();
    assertEquals(UnwritableVectorIJ.class, unitized.getClass());
    assertNotSame(unitized, twoVarCon);
    assertComponentEquals(
        new UnwritableVectorIJ(1.0 / Math.sqrt(1 + 4), new UnwritableVectorIJ(1, 2)), unitized,
        0.0);
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
  }

  @Test
  public void testCreateNegated() {
    UnwritableVectorIJ negated = twoVarCon.createNegated();
    assertEquals(UnwritableVectorIJ.class, negated.getClass());
    assertNotSame(negated, twoVarCon);
    assertEqualVector(new UnwritableVectorIJ(-1.0, new UnwritableVectorIJ(1, 2)), negated);
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
  }

  @Test
  public void testGetI() {
    assertEqualDouble(1.0, twoVarCon.getI());
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
  }

  @Test
  public void testGetJ() {
    assertEqualDouble(2.0, twoVarCon.getJ());
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
  }

  @Test
  public void testGetInt() {
    assertEqualDouble(1.0, twoVarCon.get(0));
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
    assertEqualDouble(2.0, twoVarCon.get(1));
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetIntLowException() {
    twoVarCon.get(-1);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetIntHighException() {
    twoVarCon.get(3);
  }

  @Test
  public void testGetLength() {
    assertEquivalentDouble(Math.sqrt(1 + 4), twoVarCon.getLength());
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
  }

  @Test
  public void testGetDot() {
    UnwritableVectorIJ v = new UnwritableVectorIJ(5, 7);
    assertEqualDouble(5 + 14, v.getDot(twoVarCon));
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
    assertEqualVector(new UnwritableVectorIJ(5, 7), v);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testGetSeparationZeroInstanceException() {
    VectorIJ.ZERO.getSeparation(VectorIJ.I);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testGetSeparationZeroArgumentException() {
    VectorIJ.I.getSeparation(VectorIJ.ZERO);
  }

  @Test
  public void testGetSeparation() {
    /*
     * Test some simple vectors, where the results are obvious.
     */
    assertEqualDouble(Math.PI / 2.0, VectorIJ.I.getSeparation(VectorIJ.J));

    /*
     * Now exercise some more complicated examples.
     */
    UnwritableVectorIJ a = new UnwritableVectorIJ(1, 1);
    UnwritableVectorIJ b = new UnwritableVectorIJ(1, 2);

    assertEquals(Math.acos(3.0 / Math.sqrt(2 * (1 + 4))), a.getSeparation(b), TIGHT_TOLERANCE);
    assertEqualVector(new UnwritableVectorIJ(1, 1), a);
    assertEqualVector(new UnwritableVectorIJ(1, 2), b);

    assertEquals(Math.acos(3.0 / Math.sqrt(2 * (1 + 4))), b.getSeparation(a), TIGHT_TOLERANCE);
    assertEqualVector(new UnwritableVectorIJ(1, 1), a);
    assertEqualVector(new UnwritableVectorIJ(1, 2), b);

    a = new UnwritableVectorIJ(-1, -1);

    assertEquivalentDouble(Math.acos(-3.0 / Math.sqrt(2 * (1 + 4))), a.getSeparation(b));
    assertEqualVector(new UnwritableVectorIJ(-1, -1), a);
    assertEqualVector(new UnwritableVectorIJ(1, 2), b);

    assertEquivalentDouble(Math.acos(-3.0 / Math.sqrt(2 * (1 + 4))), b.getSeparation(a));
    assertEqualVector(new UnwritableVectorIJ(-1, -1), a);
    assertEqualVector(new UnwritableVectorIJ(1, 2), b);
  }

  @Test
  public void testCopyOf() {

    UnwritableVectorIJ unwritable = new UnwritableVectorIJ(1, 2);
    UnwritableVectorIJ notUnwritable = new UnwritableVectorIJ(4, 5) {};

    UnwritableVectorIJ result = UnwritableVectorIJ.copyOf(unwritable);
    assertSame(result, unwritable);

    result = UnwritableVectorIJ.copyOf(notUnwritable);
    assertNotSame(result, notUnwritable);
    assertEquals(result, notUnwritable);
    assertEquals(UnwritableVectorIJ.class, result.getClass());

  }

  @Test
  public void testEqualsObject() {
    assertTrue(twoVarCon.equals(copyCon));
    assertNotSame(twoVarCon, copyCon);
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
    assertEqualVector(new UnwritableVectorIJ(1, 2), copyCon);

    assertTrue(twoVarCon.equals(twoVarCon));
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);

    assertFalse(twoVarCon.equals(null));
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);

    assertFalse(twoVarCon.equals(new String()));
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);

    assertFalse(twoVarCon.equals(scaleCon));
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
    assertEqualVector(new UnwritableVectorIJ(-5, -10), scaleCon);

    UnwritableVectorIJ subClass = new UnwritableVectorIJ(1, 2) {

    };
    assertTrue(twoVarCon.equals(subClass));
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
    assertEqualVector(new UnwritableVectorIJ(1, 2), subClass);

    assertTrue(subClass.equals(twoVarCon));
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
    assertEqualVector(new UnwritableVectorIJ(1, 2), subClass);

  }

  @Test
  public void testToString() {
    assertEquals("[1.0,2.0]", twoVarCon.toString());
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
  }

}
