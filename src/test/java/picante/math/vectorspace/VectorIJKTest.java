package picante.math.vectorspace;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEqualVector;
import static picante.junit.AssertTools.assertEquivalentVector;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.ImmutableList;
import picante.junit.AssertTools;
import picante.units.FundamentalPhysicalConstants;

public class VectorIJKTest {

  private static final double TIGHT_TOLERANCE = 1e-15;

  private VectorIJK defaultCon;
  private VectorIJK threeVarCon;
  private VectorIJK copyCon;
  private VectorIJK scaleCon;

  private VectorIJK a;
  private VectorIJK b;
  private VectorIJK c;
  private VectorIJK d;
  private VectorIJK e;
  private VectorIJK f;
  private VectorIJK g;
  private VectorIJK h;

  /**
   * Simple reference to PI/2, so we don't have to compute it repeatedly in methods that require it.
   */
  private static final double HALFPI = Math.PI / 2.0;

  @Before
  public void setUp() throws Exception {
    defaultCon = new VectorIJK();
    threeVarCon = new VectorIJK(-1, -3, -5);
    copyCon = new VectorIJK(threeVarCon);
    scaleCon = new VectorIJK(10, threeVarCon);

    a = new VectorIJK(1, 2, 3);
    b = new VectorIJK(4, 5, 6);
    c = new VectorIJK(1, -1, 2);
    d = new VectorIJK(-3, 4, -1);
    e = new VectorIJK(.1, 0.2, 0.3);
    f = new VectorIJK(-.01, -.02, -.03);
    g = new VectorIJK(.001, .002, .003);
    h = new VectorIJK(-.0001, -.0002, -.0003);
  }

  @After
  public void tearDown() throws Exception {
    checkStaticFinalMembers();
  }

  @Test
  public void testVectorIJK() {
    assertEqualVector(VectorIJK.ZERO, defaultCon);
  }

  @Test
  public void testVectorIJKDoubleDoubleDouble() {
    assertEqualVector(new VectorIJK(-1, -3, -5), threeVarCon);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testVectorIJKDoubleArrayIndexException() {
    new VectorIJK(new double[1]);
  }

  @Test
  public void testVectorIJKDoubleArray() {
    double[] data = {1.0, 5.5, 7.1};
    VectorIJK dataCon = new VectorIJK(data);
    assertEqualVector(new UnwritableVectorIJK(1, 5.5, 7.1), dataCon);
    AssertTools.assertEqualDouble(1.0, data[0]);
    AssertTools.assertEqualDouble(5.5, data[1]);
    AssertTools.assertEqualDouble(7.1, data[2]);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testVectorIJKIntDoubleArrayIndexException() {
    new VectorIJK(20, new double[10]);
  }

  @Test
  public void testVectorIJKIntDoubleArray() {
    double[] data = {1.0, 2.0, 3.0, 5.0, 7.0, 11.0};
    VectorIJK dataOffCon = new VectorIJK(2, data);
    assertEqualVector(new UnwritableVectorIJK(3.0, 5.0, 7.0), dataOffCon);
    AssertTools.assertEqualDouble(1.0, data[0]);
    AssertTools.assertEqualDouble(2.0, data[1]);
    AssertTools.assertEqualDouble(3.0, data[2]);
    AssertTools.assertEqualDouble(5.0, data[3]);
    AssertTools.assertEqualDouble(7.0, data[4]);
    AssertTools.assertEqualDouble(11.0, data[5]);

  }

  @Test
  public void testVectorIJKUnwritableVectorIJK() {
    assertEqualVector(new VectorIJK(-1, -3, -5), copyCon);
    assertEqualVector(new VectorIJK(-1, -3, -5), threeVarCon);
  }

  @Test
  public void testVectorIJKDoubleUnwritableVectorIJK() {
    assertEqualVector(new UnwritableVectorIJK(-10, -30, -50), scaleCon);
    assertEqualVector(new VectorIJK(-1, -3, -5), threeVarCon);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateUnitizedZeroVectorException() {
    new VectorIJK(VectorIJK.ZERO).createUnitized();
  }

  @Test
  public void testCreateUnitized() {
    VectorIJK unitized = threeVarCon.createUnitized();
    assertEquals(VectorIJK.class, unitized.getClass());
    assertNotSame(unitized, threeVarCon);
    assertEquivalentVector(
        new UnwritableVectorIJK(1.0 / Math.sqrt(1 + 9 + 25), new UnwritableVectorIJK(-1, -3, -5)),
        unitized);
  }

  @Test
  public void testCreateNegated() {
    VectorIJK negated = threeVarCon.createNegated();
    assertEquals(VectorIJK.class, negated.getClass());
    assertNotSame(negated, threeVarCon);
    assertEquivalentVector(new UnwritableVectorIJK(1, 3, 5), negated);
  }

  @Test
  public void testScale() {
    VectorIJK result = threeVarCon.scale(-5);
    assertSame(result, threeVarCon);
    assertEqualVector(new UnwritableVectorIJK(5, 15, 25), threeVarCon);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testUnitizedZeroVectorException() {
    new VectorIJK(VectorIJK.ZERO).unitize();
  }

  @Test
  public void testUnitize() {
    VectorIJK result = threeVarCon.unitize();
    assertSame(result, threeVarCon);
    assertEquivalentVector(
        new UnwritableVectorIJK(1.0 / Math.sqrt(1 + 9 + 25), new UnwritableVectorIJK(-1, -3, -5)),
        threeVarCon);
  }

  @Test
  public void testNegate() {
    VectorIJK result = threeVarCon.negate();
    assertSame(result, threeVarCon);
    assertEqualVector(new UnwritableVectorIJK(1, 3, 5), threeVarCon);
  }

  @Test
  public void testClear() {
    VectorIJK result = threeVarCon.clear();
    assertSame(result, threeVarCon);
    assertEqualVector(VectorIJK.ZERO, threeVarCon);
  }

  @Test
  public void testSetI() {
    threeVarCon.setI(-21);
    assertEquals(-21, threeVarCon.i, 0.0);
    assertEquals(-3, threeVarCon.j, 0.0);
    assertEquals(-5, threeVarCon.k, 0.0);
  }

  @Test
  public void testSetJ() {
    threeVarCon.setJ(-23);
    assertEquals(-23, threeVarCon.j, 0.0);
    assertEquals(-1, threeVarCon.i, 0.0);
    assertEquals(-5, threeVarCon.k, 0.0);
  }

  @Test
  public void testSetK() {
    threeVarCon.setK(31);
    assertEquals(31, threeVarCon.k, 0.0);
    assertEquals(-1, threeVarCon.i, 0.0);
    assertEquals(-3, threeVarCon.j, 0.0);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testSetException() {
    threeVarCon.set(4, 10);
  }

  @Test
  public void testSet() {
    threeVarCon.set(0, 1);
    assertEqualVector(new UnwritableVectorIJK(1, -3, -5), threeVarCon);
    threeVarCon.set(1, 3);
    assertEqualVector(new UnwritableVectorIJK(1, 3, -5), threeVarCon);
    threeVarCon.set(2, 5);
    assertEqualVector(new UnwritableVectorIJK(1, 3, 5), threeVarCon);
  }

  @Test
  public void testSetToUnwritableVectorIJK() {
    VectorIJK testdata = new VectorIJK(1, 2, 3);
    VectorIJK result = threeVarCon.setTo(testdata);
    assertSame(result, threeVarCon);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), testdata);
  }

  @Test
  public void testSetToDoubleUnwritableVectorIJK() {
    VectorIJK testdata = new VectorIJK(5, 7, 13);
    VectorIJK result = threeVarCon.setTo(10, testdata);
    assertSame(result, threeVarCon);
    assertEqualVector(new UnwritableVectorIJK(50, 70, 130), threeVarCon);
    assertEqualVector(new UnwritableVectorIJK(5, 7, 13), testdata);
  }

  @Test
  public void testSetToDoubleDoubleDouble() {
    VectorIJK result = threeVarCon.setTo(21, 13, 17);
    assertSame(result, threeVarCon);
    assertEqualVector(new UnwritableVectorIJK(21, 13, 17), threeVarCon);
  }

  @Test
  public void testSetToDoubleArray() {
    VectorIJK result = threeVarCon.setTo(new double[] {1, 2, 3});
    assertSame(result, threeVarCon);
    assertEqualVector(new UnwritableVectorIJK(1, 2, 3), threeVarCon);
  }

  @Test
  public void testSetToIntegerDoubleArray() {
    VectorIJK result =
        threeVarCon.setTo(10, new double[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12});
    assertSame(result, threeVarCon);
    assertEqualVector(new UnwritableVectorIJK(10, 11, 12), threeVarCon);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testSetToUnitizedZeroVectorException() {
    new VectorIJK().setToUnitized(VectorIJK.ZERO);
  }

  @Test
  public void testSetToUnitized() {
    VectorIJK testdata = new VectorIJK(3, 5, 7);
    VectorIJK result = threeVarCon.setToUnitized(testdata);
    assertSame(result, threeVarCon);
    assertEquivalentVector(new UnwritableVectorIJK(1.0 / Math.sqrt(9.0 + 25.0 + 49.0),
        new UnwritableVectorIJK(3, 5, 7)), threeVarCon);
    assertEqualVector(new UnwritableVectorIJK(3, 5, 7), testdata);
  }

  @Test
  public void testSetToNegated() {
    VectorIJK testdata = new VectorIJK(3, -5, 7);
    VectorIJK result = threeVarCon.setToNegated(testdata);
    assertSame(result, threeVarCon);
    assertEquivalentVector(new UnwritableVectorIJK(-3, 5, -7), threeVarCon);
    assertEquivalentVector(new UnwritableVectorIJK(3, -5, 7), testdata);

  }

  @Test
  public void testPointwiseMultiply() {
    VectorIJK d = VectorIJK.pointwiseMultiply(a, b, c);
    assertSame(c, d);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(4, 10, 18), c);
  }

  @Test
  public void testAdd() {
    VectorIJK d = VectorIJK.add(a, b, c);
    assertSame(c, d);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(5, 7, 9), c);
  }

  @Test
  public void testAddOverA() {
    VectorIJK d = VectorIJK.add(a, b, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJK(5, 7, 9), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
  }

  @Test
  public void testAddOverB() {
    VectorIJK d = VectorIJK.add(a, b, b);
    assertSame(d, b);
    assertEqualVector(new VectorIJK(5, 7, 9), b);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
  }

  @Test
  public void testAddAAOverA() {
    VectorIJK d = VectorIJK.add(a, a, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJK(2, 4, 6), a);
  }

  @Test
  public void testNewAdd() {
    VectorIJK d = VectorIJK.add(a, b);
    assertNotSame(a, d);
    assertNotSame(b, d);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(5, 7, 9), d);
  }

  @Test
  public void testAddRSS() {
    VectorIJK d = VectorIJK.addRSS(a, b, c);
    assertSame(c, d);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertComponentEquals(new VectorIJK(Math.sqrt(17), Math.sqrt(29), Math.sqrt(45)), c,
        TIGHT_TOLERANCE);
  }

  @Test
  public void testAddRSSOverA() {
    VectorIJK d = VectorIJK.addRSS(a, b, a);
    assertSame(d, a);
    assertComponentEquals(new VectorIJK(Math.sqrt(17), Math.sqrt(29), Math.sqrt(45)), a,
        TIGHT_TOLERANCE);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
  }

  @Test
  public void testAddRSSOverB() {
    VectorIJK d = VectorIJK.addRSS(a, b, b);
    assertSame(d, b);
    assertComponentEquals(new VectorIJK(Math.sqrt(17), Math.sqrt(29), Math.sqrt(45)), b,
        TIGHT_TOLERANCE);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
  }

  @Test
  public void testAddRSSAAOverA() {
    VectorIJK d = VectorIJK.addRSS(a, a, a);
    assertSame(d, a);
    assertComponentEquals(new VectorIJK(Math.sqrt(2), Math.sqrt(8), Math.sqrt(18)), a,
        TIGHT_TOLERANCE);
  }

  @Test
  public void testNewAddRSS() {
    VectorIJK d = VectorIJK.addRSS(a, b);
    assertNotSame(a, d);
    assertNotSame(b, d);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertComponentEquals(new VectorIJK(Math.sqrt(17), Math.sqrt(29), Math.sqrt(45)), d,
        TIGHT_TOLERANCE);
  }

  @Test
  public void testAddAll() {
    VectorIJK d = VectorIJK.addAll(ImmutableList.of(a, b), c);
    assertSame(c, d);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(5, 7, 9), c);
  }

  @Test
  public void testAddAllOverA() {
    VectorIJK d = VectorIJK.addAll(ImmutableList.of(a, b), a);
    assertSame(d, a);
    assertEqualVector(new VectorIJK(5, 7, 9), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
  }

  @Test
  public void testAddAllOverB() {
    VectorIJK d = VectorIJK.addAll(ImmutableList.of(a, b), b);
    assertSame(d, b);
    assertEqualVector(new VectorIJK(5, 7, 9), b);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
  }

  @Test
  public void testAddAllAAOverA() {
    VectorIJK d = VectorIJK.addAll(ImmutableList.of(a, a), a);
    assertSame(d, a);
    assertEqualVector(new VectorIJK(2, 4, 6), a);
  }

  @Test
  public void testNewAddAll() {
    VectorIJK d = VectorIJK.addAll(ImmutableList.of(a, b));
    assertNotSame(a, d);
    assertNotSame(b, d);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(5, 7, 9), d);
  }

  @Test
  public void testSubtract() {
    VectorIJK d = VectorIJK.subtract(a, c, b);
    assertSame(d, b);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(0, 3, 1), b);
  }

  @Test
  public void testSubtractOverA() {
    VectorIJK d = VectorIJK.subtract(a, c, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(0, 3, 1), a);
  }

  @Test
  public void testSubtractOverC() {
    VectorIJK d = VectorIJK.subtract(a, c, c);
    assertSame(d, c);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(0, 3, 1), c);
  }

  @Test
  public void testSubtractAAOverA() {
    VectorIJK d = VectorIJK.subtract(a, a, a);
    assertSame(d, a);
    assertEqualVector(VectorIJK.ZERO, a);
  }

  @Test
  public void testNewSubtract() {
    VectorIJK d = VectorIJK.subtract(a, c);
    assertNotSame(d, a);
    assertNotSame(d, c);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(0, 3, 1), d);
  }

  @Test
  public void testCross() {
    VectorIJK d = VectorIJK.cross(a, c, b);
    assertSame(d, b);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(7, 1, -3), b);
  }

  @Test
  public void testCrossOverA() {
    VectorIJK d = VectorIJK.cross(a, c, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(7, 1, -3), a);
  }

  @Test
  public void testCrossOverC() {
    VectorIJK d = VectorIJK.cross(a, c, c);
    assertSame(d, c);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(7, 1, -3), c);
  }

  @Test
  public void testCrossAAOverA() {
    VectorIJK d = VectorIJK.cross(a, a, a);
    assertSame(d, a);
    assertEqualVector(VectorIJK.ZERO, a);
  }

  @Test
  public void testNewCross() {
    VectorIJK d = VectorIJK.cross(a, c);
    assertNotSame(d, a);
    assertNotSame(d, c);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(7, 1, -3), d);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testUCrossZeroLeftVectorException() {
    VectorIJK.uCross(VectorIJK.ZERO, a, b);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testUCrossZeroRightVectorException() {
    VectorIJK.uCross(a, VectorIJK.ZERO, b);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testUCrossZeroResultException() {
    VectorIJK.uCross(a, a, b);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testUCrossAAOverA() {
    VectorIJK.uCross(a, a, a);
  }

  @Test
  public void testUCross() {
    VectorIJK d = VectorIJK.uCross(a, c, b);
    assertSame(d, b);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertComponentEquals(new VectorIJK(1.0 / Math.sqrt(59.0), new VectorIJK(7, 1, -3)), b,
        TIGHT_TOLERANCE);
  }

  @Test
  public void testUCrossOverA() {
    VectorIJK d = VectorIJK.uCross(a, c, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertComponentEquals(new VectorIJK(1.0 / Math.sqrt(59.0), new VectorIJK(7, 1, -3)), a,
        TIGHT_TOLERANCE);
  }

  @Test
  public void testUCrossOverC() {
    VectorIJK d = VectorIJK.uCross(a, c, c);
    assertSame(d, c);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertComponentEquals(new VectorIJK(1.0 / Math.sqrt(59.0), new VectorIJK(7, 1, -3)), c,
        TIGHT_TOLERANCE);
  }

  @Test
  public void testNewUCross() {
    VectorIJK d = VectorIJK.uCross(a, c);
    assertNotSame(d, a);
    assertNotSame(d, c);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertComponentEquals(new VectorIJK(1.0 / Math.sqrt(59.0), new VectorIJK(7, 1, -3)), d,
        TIGHT_TOLERANCE);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJK() {
    VectorIJK d = VectorIJK.combine(2.0, a, -3.0, b, c);
    assertSame(d, c);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(-10, -11, -12), c);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJKOverA() {
    VectorIJK d = VectorIJK.combine(2.0, a, -3.0, b, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(-10, -11, -12), a);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJKOverB() {
    VectorIJK d = VectorIJK.combine(2.0, a, -3.0, b, b);
    assertSame(d, b);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(-10, -11, -12), b);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJKAAOverA() {
    VectorIJK d = VectorIJK.combine(2.0, a, -1.0, a, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
  }

  @Test
  public void testNewCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJK() {
    VectorIJK d = VectorIJK.combine(2.0, a, -3.0, b);
    assertNotSame(d, a);
    assertNotSame(d, b);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(-10, -11, -12), d);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJK() {
    VectorIJK d = new VectorIJK();
    VectorIJK e = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, d);
    assertSame(e, d);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-9, -12, -10), d);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJKOverA() {
    VectorIJK e = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, a);
    assertSame(e, a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-9, -12, -10), a);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJKOverB() {
    VectorIJK e = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, b);
    assertSame(e, b);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-9, -12, -10), b);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJKOverC() {
    VectorIJK e = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, c);
    assertSame(e, c);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(-9, -12, -10), c);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJKAAOverA() {
    VectorIJK e = VectorIJK.combine(1.0, a, -3.0, a, 4.0, a, a);
    assertSame(e, a);
    assertEqualVector(new VectorIJK(2, 4, 6), a);
  }

  @Test
  public void testNewCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJK() {
    VectorIJK e = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c);
    assertNotSame(e, a);
    assertNotSame(e, b);
    assertNotSame(e, c);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-9, -12, -10), e);
  }

  //
  @Test
  public void testCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJK() {
    VectorIJK d = new VectorIJK(-3, 4, -1);
    VectorIJK e = new VectorIJK();
    VectorIJK f = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, -2.0, d, e);
    assertSame(f, e);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-3, 4, -1), d);
    assertEqualVector(new VectorIJK(-3, -20, -8), e);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJKOverA() {
    VectorIJK d = new VectorIJK(-3, 4, -1);
    VectorIJK f = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, -2.0, d, a);
    assertSame(f, a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-3, 4, -1), d);
    assertEqualVector(new VectorIJK(-3, -20, -8), a);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJKOverB() {
    VectorIJK d = new VectorIJK(-3, 4, -1);
    VectorIJK f = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, -2.0, d, b);
    assertSame(f, b);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-3, -20, -8), b);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJKOverC() {
    VectorIJK d = new VectorIJK(-3, 4, -1);
    VectorIJK f = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, -2.0, d, c);
    assertSame(f, c);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(-3, -20, -8), c);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJKOverD() {
    VectorIJK d = new VectorIJK(-3, 4, -1);
    VectorIJK f = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, -2.0, d, d);
    assertSame(f, d);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-3, -20, -8), d);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJKAAAOverA() {
    VectorIJK f = VectorIJK.combine(1.0, a, -3.0, a, 4.0, a, -3.0, a, a);
    assertSame(f, a);
    assertEqualVector(new VectorIJK(-1, -2, -3), a);
  }

  @Test
  public void testNewCombineDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKVectorIJK() {
    VectorIJK d = new VectorIJK(-3, 4, -1);
    VectorIJK f = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, -2.0, d);
    assertNotSame(f, a);
    assertNotSame(f, b);
    assertNotSame(f, c);
    assertNotSame(f, d);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-3, 4, -1), d);
    assertEqualVector(new VectorIJK(-3, -20, -8), f);
  }

  @Test
  public void testNewCombine5() {
    VectorIJK f = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, -2.0, d, -1.0, e);
    assertNotSame(f, a);
    assertNotSame(f, b);
    assertNotSame(f, c);
    assertNotSame(f, d);
    assertNotSame(f, e);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-3, 4, -1), d);
    assertEqualVector(new VectorIJK(.1, 0.2, 0.3), e);
    assertEqualVector(new VectorIJK(-3.1, -20.2, -8.3), f);
  }

  @Test
  public void testNewCombine5Buffer() {
    VectorIJK f = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, -2.0, d, -1.0, e, new VectorIJK());
    assertNotSame(f, a);
    assertNotSame(f, b);
    assertNotSame(f, c);
    assertNotSame(f, d);
    assertNotSame(f, e);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-3, 4, -1), d);
    assertEqualVector(new VectorIJK(.1, .2, .3), e);
    assertEqualVector(new VectorIJK(-3.1, -20.2, -8.3), f);
  }

  @Test
  public void testNewCombine6() {
    VectorIJK g = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, -2.0, d, -1.0, e, 2.0, f);
    assertNotSame(g, a);
    assertNotSame(g, b);
    assertNotSame(g, c);
    assertNotSame(g, d);
    assertNotSame(g, e);
    assertNotSame(g, f);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-3, 4, -1), d);
    assertEqualVector(new VectorIJK(.1, .2, .3), e);
    assertEqualVector(new VectorIJK(-.01, -.02, -.03), f);
    assertEquivalentVector(new VectorIJK(-3.12, -20.24, -8.36), g);
  }

  @Test
  public void testNewCombine6Buffer() {
    VectorIJK g =
        VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, -2.0, d, -1.0, e, 2.0, f, new VectorIJK());
    assertNotSame(g, a);
    assertNotSame(g, b);
    assertNotSame(g, c);
    assertNotSame(g, d);
    assertNotSame(g, e);
    assertNotSame(g, f);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-3, 4, -1), d);
    assertEqualVector(new VectorIJK(.1, .2, .3), e);
    assertEqualVector(new VectorIJK(-.01, -.02, -.03), f);
    assertEquivalentVector(new VectorIJK(-3.12, -20.24, -8.36), g);
  }

  @Test
  public void testNewCombine7() {
    VectorIJK h = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, -2.0, d, -1.0, e, 2.0, f, -3.0, g);
    assertNotSame(h, a);
    assertNotSame(h, b);
    assertNotSame(h, c);
    assertNotSame(h, d);
    assertNotSame(h, e);
    assertNotSame(h, f);
    assertNotSame(h, g);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-3, 4, -1), d);
    assertEqualVector(new VectorIJK(.1, .2, .3), e);
    assertEqualVector(new VectorIJK(-.01, -.02, -.03), f);
    assertEqualVector(new VectorIJK(.001, .002, .003), g);
    assertEquivalentVector(new VectorIJK(-3.123, -20.246, -8.369), h);
  }

  @Test
  public void testNewCombine7Buffer() {
    VectorIJK h = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, -2.0, d, -1.0, e, 2.0, f, -3.0, g,
        new VectorIJK());
    assertNotSame(h, a);
    assertNotSame(h, b);
    assertNotSame(h, c);
    assertNotSame(h, d);
    assertNotSame(h, e);
    assertNotSame(h, f);
    assertNotSame(h, g);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-3, 4, -1), d);
    assertEqualVector(new VectorIJK(.1, .2, .3), e);
    assertEqualVector(new VectorIJK(-.01, -.02, -.03), f);
    assertEqualVector(new VectorIJK(.001, .002, .003), g);
    assertEquivalentVector(new VectorIJK(-3.123, -20.246, -8.369), h);
  }

  @Test
  public void testNewCombine8() {
    VectorIJK i =
        VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, -2.0, d, -1.0, e, 2.0, f, -3.0, g, 4.0, h);
    assertNotSame(i, a);
    assertNotSame(i, b);
    assertNotSame(i, c);
    assertNotSame(i, d);
    assertNotSame(i, e);
    assertNotSame(i, f);
    assertNotSame(i, g);
    assertNotSame(i, h);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-3, 4, -1), d);
    assertEqualVector(new VectorIJK(.1, .2, .3), e);
    assertEqualVector(new VectorIJK(-.01, -.02, -.03), f);
    assertEqualVector(new VectorIJK(.001, .002, .003), g);
    assertEqualVector(new VectorIJK(-.0001, -.0002, -.0003), h);
    assertEquivalentVector(new VectorIJK(-3.1234, -20.2468, -8.3702), i);
  }

  @Test
  public void testNewCombine8Buffer() {
    VectorIJK i = VectorIJK.combine(2.0, a, -3.0, b, 1.0, c, -2.0, d, -1.0, e, 2.0, f, -3.0, g, 4.0,
        h, new VectorIJK());
    assertNotSame(i, a);
    assertNotSame(i, b);
    assertNotSame(i, c);
    assertNotSame(i, d);
    assertNotSame(i, e);
    assertNotSame(i, f);
    assertNotSame(i, g);
    assertNotSame(i, h);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEqualVector(new VectorIJK(1, -1, 2), c);
    assertEqualVector(new VectorIJK(-3, 4, -1), d);
    assertEqualVector(new VectorIJK(.1, .2, .3), e);
    assertEqualVector(new VectorIJK(-.01, -.02, -.03), f);
    assertEqualVector(new VectorIJK(.001, .002, .003), g);
    assertEqualVector(new VectorIJK(-.0001, -.0002, -.0003), h);
    assertEquivalentVector(new VectorIJK(-3.1234, -20.2468, -8.3702), i);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testProjectZeroRightVectorException() {
    VectorIJK.project(a, VectorIJK.ZERO, b);
  }

  @Test
  public void testProjectZeroLeftVector() {
    VectorIJK d = VectorIJK.project(VectorIJK.ZERO, a, b);
    assertSame(d, b);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(VectorIJK.ZERO, b);
  }

  @Test
  public void testProject() {
    VectorIJK d = VectorIJK.project(a, b, c);
    assertSame(d, c);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEquivalentVector(new VectorIJK(32.0 / 77.0, b), c);
  }

  @Test
  public void testProjectOverA() {
    VectorIJK d = VectorIJK.project(a, b, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEquivalentVector(new VectorIJK(32.0 / 77.0, b), a);
  }

  @Test
  public void testProjectOverB() {
    c.setTo(b);
    VectorIJK d = VectorIJK.project(a, b, b);
    assertSame(d, b);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEquivalentVector(new VectorIJK(32.0 / 77.0, c), b);
  }

  @Test
  public void testProjectAAOverA() {
    VectorIJK d = VectorIJK.project(a, a, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
  }

  @Test
  public void testNewProject() {
    VectorIJK d = VectorIJK.project(a, b);
    assertNotSame(d, a);
    assertNotSame(d, b);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertEquivalentVector(new VectorIJK(32.0 / 77.0, b), d);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testPlaneProjectZeroNormalException() {
    VectorIJK.planeProject(a, VectorIJK.ZERO, b);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testPlaneProjectZeroNormalZeroVectorException() {
    VectorIJK.planeProject(VectorIJK.ZERO, VectorIJK.ZERO, a);
  }

  @Test
  public void testPlaneProjectZeroVector() {
    VectorIJK d = VectorIJK.planeProject(VectorIJK.ZERO, a, b);
    assertSame(d, b);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(0, 0, 0), b);
  }

  @Test
  public void testPlaneProject() {
    VectorIJK d = VectorIJK.planeProject(a, b, c);
    assertSame(d, c);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertComponentEquals(new VectorIJK(-51.0 / 77.0, -6.0 / 77.0, 39.0 / 77.0), c,
        TIGHT_TOLERANCE);
  }

  @Test
  public void testPlaneProjectOverA() {
    VectorIJK d = VectorIJK.planeProject(a, b, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertComponentEquals(new VectorIJK(-51.0 / 77.0, -6.0 / 77.0, 39.0 / 77.0), a,
        TIGHT_TOLERANCE);
  }

  @Test
  public void testPlaneProjectOverB() {
    VectorIJK d = VectorIJK.planeProject(a, b, b);
    assertSame(d, b);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertComponentEquals(new VectorIJK(-51.0 / 77.0, -6.0 / 77.0, 39.0 / 77.0), b,
        TIGHT_TOLERANCE);
  }

  @Test
  public void testPlaneProjectAAOverA() {
    VectorIJK d = VectorIJK.planeProject(a, a, a);
    assertSame(d, a);
    assertEqualVector(VectorIJK.ZERO, a);
  }

  @Test
  public void testNewPlaneProject() {
    VectorIJK d = VectorIJK.planeProject(a, b);
    assertNotSame(d, a);
    assertNotSame(d, b);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEqualVector(new VectorIJK(4, 5, 6), b);
    assertComponentEquals(new VectorIJK(-51.0 / 77.0, -6.0 / 77.0, 39.0 / 77.0), d,
        TIGHT_TOLERANCE);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testRotateZeroVectorAboutZeroVectorException() {
    VectorIJK.rotate(VectorIJK.ZERO, VectorIJK.ZERO, Math.toRadians(30.0), a);
  }

  @Test
  public void testRotateZeroVectorException() {
    VectorIJK b = VectorIJK.rotate(VectorIJK.ZERO, VectorIJK.I, Math.toRadians(30.0), a);
    assertSame(b, a);
    assertEqualVector(VectorIJK.ZERO, a);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testRotateVectorAboutZeroVectorException() {
    VectorIJK.rotate(VectorIJK.I, VectorIJK.ZERO, Math.toRadians(30.0), a);
  }

  @Test
  public void testRotateExamples() {
    a.setTo(1, 2, 3);
    VectorIJK d = VectorIJK.rotate(a, VectorIJK.K, FundamentalPhysicalConstants.HALFPI, c);
    assertSame(d, c);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
    assertEquivalentVector(new VectorIJK(-2, 1, 3), c);

    d = VectorIJK.rotate(VectorIJK.I, VectorIJK.K, FundamentalPhysicalConstants.HALFPI, c);
    assertSame(d, c);
    assertComponentEquals(VectorIJK.J, c, TIGHT_TOLERANCE);

    d = VectorIJK.rotate(VectorIJK.J, VectorIJK.K, FundamentalPhysicalConstants.HALFPI, c);
    assertSame(d, c);
    assertComponentEquals(VectorIJK.MINUS_I, c, TIGHT_TOLERANCE);
  }

  @Test
  public void testRotateOverA() {

    /*
     * Hold independent copies of a and c so we can easily refer to them within our test.
     */
    VectorIJK copyA = new VectorIJK(a);
    VectorIJK copyC = new VectorIJK(c);

    VectorIJK d = VectorIJK.rotate(a, c, Math.toRadians(32.0), a);
    assertSame(d, a);

    /*
     * Check that rotate did not mutate either a or c.
     */
    assertEqualVector(copyC, c);

    /*
     * Since this is a rotation, it preserves inner products. Verify that the separation between a
     * and c prior to rotation is the same as that after the rotation.
     */
    assertEquals(copyA.getSeparation(copyC), d.getSeparation(copyC), TIGHT_TOLERANCE);

    /*
     * Again, since it preserves inner products, the length of a should not have changed under the
     * rotation.
     */
    AssertTools.assertEquivalentDouble(copyA.getLength(), d.getLength());

    /*
     * Lastly, verify that it did indeed rotate the vectors by the appropriate number of degrees. To
     * properly validate this, project the vectors into a plane normal to the original c.
     */
    VectorIJK ta = VectorIJK.planeProject(copyA, copyC, new VectorIJK());
    VectorIJK tr = VectorIJK.planeProject(d, copyC, new VectorIJK());

    AssertTools.assertEquivalentDouble(Math.toRadians(32.0), ta.getSeparation(tr));

    /*
     * Lastly verify the sense of the rotation was as expected. A positive rotation should orient
     * the vectors such that the following expression is true.
     */
    assertEquals(0.0, VectorIJK.cross(ta, tr, new VectorIJK()).getSeparation(copyC),
        TIGHT_TOLERANCE);
  }

  @Test
  public void testRotateOverC() {

    /*
     * Hold independent copies of a and c so we can easily refer to them within our test.
     */
    VectorIJK copyA = new VectorIJK(a);
    VectorIJK copyC = new VectorIJK(c);

    VectorIJK d = VectorIJK.rotate(a, c, Math.toRadians(32.0), c);
    assertSame(d, c);

    /*
     * Check that rotate did not mutate either a or c.
     */
    assertEqualVector(copyA, a);

    /*
     * Since this is a rotation, it preserves inner products. Verify that the separation between a
     * and c prior to rotation is the same as that after the rotation.
     */
    assertEquals(copyA.getSeparation(copyC), d.getSeparation(copyC), TIGHT_TOLERANCE);

    /*
     * Again, since it preserves inner products, the length of a should not have changed under the
     * rotation.
     */
    AssertTools.assertEquivalentDouble(copyA.getLength(), d.getLength());

    /*
     * Lastly, verify that it did indeed rotate the vectors by the appropriate number of degrees. To
     * properly validate this, project the vectors into a plane normal to the original c.
     */
    VectorIJK ta = VectorIJK.planeProject(copyA, copyC, new VectorIJK());
    VectorIJK tr = VectorIJK.planeProject(d, copyC, new VectorIJK());

    AssertTools.assertEquivalentDouble(Math.toRadians(32.0), ta.getSeparation(tr));

    /*
     * Lastly verify the sense of the rotation was as expected. A positive rotation should orient
     * the vectors such that the following expression is true.
     */
    assertEquals(0.0, VectorIJK.cross(ta, tr, new VectorIJK()).getSeparation(copyC),
        TIGHT_TOLERANCE);
  }

  @Test
  public void testRotateAAOverA() {
    VectorIJK d = VectorIJK.rotate(a, a, Math.toRadians(HALFPI), a);
    assertSame(d, a);
    assertEqualVector(new VectorIJK(1, 2, 3), a);
  }

  @Test
  public void testRotate() {

    /*
     * Hold independent copies of a and c so we can easily refer to them within our test.
     */
    VectorIJK copyA = new VectorIJK(a);
    VectorIJK copyC = new VectorIJK(c);

    VectorIJK d = VectorIJK.rotate(a, c, Math.toRadians(32.0), b);
    assertSame(d, b);

    /*
     * Check that rotate did not mutate either a or c.
     */
    assertEqualVector(copyA, a);
    assertEqualVector(copyC, c);

    /*
     * Since this is a rotation, it preserves inner products. Verify that the separation between a
     * and c prior to rotation is the same as that after the rotation.
     */
    assertEquals(copyA.getSeparation(copyC), d.getSeparation(copyC), TIGHT_TOLERANCE);

    /*
     * Again, since it preserves inner products, the length of a should not have changed under the
     * rotation.
     */
    AssertTools.assertEquivalentDouble(copyA.getLength(), d.getLength());

    /*
     * Lastly, verify that it did indeed rotate the vectors by the appropriate number of degrees. To
     * properly validate this, project the vectors into a plane normal to the original c.
     */
    VectorIJK ta = VectorIJK.planeProject(copyA, copyC, new VectorIJK());
    VectorIJK tr = VectorIJK.planeProject(d, copyC, new VectorIJK());

    AssertTools.assertEquivalentDouble(Math.toRadians(32.0), ta.getSeparation(tr));

    /*
     * Lastly verify the sense of the rotation was as expected. A positive rotation should orient
     * the vectors such that the following expression is true.
     */
    assertEquals(0.0, VectorIJK.cross(ta, tr, new VectorIJK()).getSeparation(copyC),
        TIGHT_TOLERANCE);
  }

  @Test
  public void testNewRotate() {

    /*
     * Hold independent copies of a and c so we can easily refer to them within our test.
     */
    VectorIJK copyA = new VectorIJK(a);
    VectorIJK copyC = new VectorIJK(c);

    VectorIJK d = VectorIJK.rotate(a, c, Math.toRadians(32.0));
    assertNotSame(d, a);
    assertNotSame(d, c);

    /*
     * Check that rotate did not mutate either a or c.
     */
    assertEqualVector(copyA, a);
    assertEqualVector(copyC, c);

    /*
     * Since this is a rotation, it preserves inner products. Verify that the separation between a
     * and c prior to rotation is the same as that after the rotation.
     */
    assertEquals(copyA.getSeparation(copyC), d.getSeparation(copyC), TIGHT_TOLERANCE);

    /*
     * Again, since it preserves inner products, the length of a should not have changed under the
     * rotation.
     */
    AssertTools.assertEquivalentDouble(copyA.getLength(), d.getLength());

    /*
     * Lastly, verify that it did indeed rotate the vectors by the appropriate number of degrees. To
     * properly validate this, project the vectors into a plane normal to the original c.
     */
    VectorIJK ta = VectorIJK.planeProject(copyA, copyC, new VectorIJK());
    VectorIJK tr = VectorIJK.planeProject(d, copyC, new VectorIJK());

    AssertTools.assertEquivalentDouble(Math.toRadians(32.0), ta.getSeparation(tr));

    /*
     * Lastly verify the sense of the rotation was as expected. A positive rotation should orient
     * the vectors such that the following expression is true.
     */
    assertEquals(0.0, VectorIJK.cross(ta, tr, new VectorIJK()).getSeparation(copyC),
        TIGHT_TOLERANCE);
  }

  static void checkStaticFinalMembers() {

    assertEqualVector(new UnwritableVectorIJK(0, 0, 0), VectorIJK.ZERO);
    assertEqualVector(new UnwritableVectorIJK(1, 0, 0), VectorIJK.I);
    assertEqualVector(new UnwritableVectorIJK(0, 1, 0), VectorIJK.J);
    assertEqualVector(new UnwritableVectorIJK(0, 0, 1), VectorIJK.K);
    assertEqualVector(new UnwritableVectorIJK(-1, 0, 0), VectorIJK.MINUS_I);
    assertEqualVector(new UnwritableVectorIJK(0, -1, 0), VectorIJK.MINUS_J);
    assertEqualVector(new UnwritableVectorIJK(0, 0, -1), VectorIJK.MINUS_K);

  }

}
