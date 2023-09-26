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

public class VectorIJTest {

  private static final double TIGHT_TOLERANCE = 1e-15;

  private VectorIJ defaultCon;
  private VectorIJ twoVarCon;
  private VectorIJ copyCon;
  private VectorIJ scaleCon;

  private VectorIJ a;
  private VectorIJ b;
  private VectorIJ c;

  private VectorIJK bIJK;

  @Before
  public void setUp() throws Exception {
    defaultCon = new VectorIJ();
    twoVarCon = new VectorIJ(-1, -3);
    copyCon = new VectorIJ(twoVarCon);
    scaleCon = new VectorIJ(10, twoVarCon);

    a = new VectorIJ(1, 2);
    b = new VectorIJ(4, 5);
    c = new VectorIJ(1, -1);

    bIJK = new VectorIJK(0, 0, -3);
  }

  @After
  public void tearDown() throws Exception {
    checkStaticFinalMembers();
  }

  @Test
  public void testVectorIJ() {
    assertEqualVector(VectorIJ.ZERO, defaultCon);
  }

  @Test
  public void testVectorIJDoubleDouble() {
    assertEqualVector(new VectorIJ(-1, -3), twoVarCon);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testVectorIJDoubleArrayIndexException() {
    new VectorIJ(new double[1]);
  }

  @Test
  public void testVectorIJDoubleArray() {
    double[] data = {1.0, 5.5};
    VectorIJ dataCon = new VectorIJ(data);
    assertEqualVector(new UnwritableVectorIJ(1, 5.5), dataCon);
    AssertTools.assertEqualDouble(1.0, data[0]);
    AssertTools.assertEqualDouble(5.5, data[1]);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testVectorIJIntDoubleArrayIndexException() {
    new VectorIJ(20, new double[10]);
  }

  @Test
  public void testVectorIJIntDoubleArray() {
    double[] data = {1.0, 2.0, 3.0, 5.0, 7.0, 11.0};
    VectorIJ dataOffCon = new VectorIJ(2, data);
    assertEqualVector(new UnwritableVectorIJ(3.0, 5.0), dataOffCon);
    AssertTools.assertEqualDouble(1.0, data[0]);
    AssertTools.assertEqualDouble(2.0, data[1]);
    AssertTools.assertEqualDouble(3.0, data[2]);
    AssertTools.assertEqualDouble(5.0, data[3]);
    AssertTools.assertEqualDouble(7.0, data[4]);
    AssertTools.assertEqualDouble(11.0, data[5]);
  }

  @Test
  public void testVectorIJUnwritableVectorIJ() {
    assertEqualVector(new VectorIJ(-1, -3), copyCon);
    assertEqualVector(new VectorIJ(-1, -3), twoVarCon);
  }

  @Test
  public void testVectorIJDoubleUnwritableVectorIJ() {
    assertEqualVector(new UnwritableVectorIJ(-10, -30), scaleCon);
    assertEqualVector(new VectorIJ(-1, -3), twoVarCon);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateUnitizedZeroVectorException() {
    new VectorIJ(VectorIJ.ZERO).createUnitized();
  }

  @Test
  public void testCreateUnitized() {
    VectorIJ unitized = twoVarCon.createUnitized();
    assertEquals(VectorIJ.class, unitized.getClass());
    assertNotSame(unitized, twoVarCon);
    assertEquivalentVector(
        new UnwritableVectorIJ(1.0 / Math.sqrt(1 + 9), new UnwritableVectorIJ(-1, -3)), unitized);
  }

  @Test
  public void testCreateNegated() {
    VectorIJ negated = twoVarCon.createNegated();
    assertEquals(VectorIJ.class, negated.getClass());
    assertNotSame(negated, twoVarCon);
    assertEquivalentVector(new UnwritableVectorIJ(1, 3), negated);
  }

  @Test
  public void testScale() {
    VectorIJ result = twoVarCon.scale(-5);
    assertSame(result, twoVarCon);
    assertEqualVector(new UnwritableVectorIJ(5, 15), twoVarCon);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testUnitizedZeroVectorException() {
    new VectorIJ(VectorIJ.ZERO).unitize();
  }

  @Test
  public void testUnitize() {
    VectorIJ result = twoVarCon.unitize();
    assertSame(result, twoVarCon);
    assertEquivalentVector(
        new UnwritableVectorIJ(1.0 / Math.sqrt(1 + 9), new UnwritableVectorIJ(-1, -3)), twoVarCon);
  }

  @Test
  public void testNegate() {
    VectorIJ result = twoVarCon.negate();
    assertSame(result, twoVarCon);
    assertEqualVector(new UnwritableVectorIJ(1, 3), twoVarCon);
  }

  @Test
  public void testClear() {
    VectorIJ result = twoVarCon.clear();
    assertSame(result, twoVarCon);
    assertEqualVector(VectorIJ.ZERO, twoVarCon);
  }

  @Test
  public void testSetI() {
    twoVarCon.setI(-21);
    assertEquals(-21, twoVarCon.i, 0.0);
    assertEquals(-3, twoVarCon.j, 0.0);
  }

  @Test
  public void testSetJ() {
    twoVarCon.setJ(-23);
    assertEquals(-23, twoVarCon.j, 0.0);
    assertEquals(-1, twoVarCon.i, 0.0);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testSetException() {
    twoVarCon.set(3, 10);
  }

  @Test
  public void testSet() {
    twoVarCon.set(0, 1);
    assertEqualVector(new UnwritableVectorIJ(1, -3), twoVarCon);
    twoVarCon.set(1, 3);
    assertEqualVector(new UnwritableVectorIJ(1, 3), twoVarCon);
  }

  @Test
  public void testSetToUnwritableVectorIJ() {
    VectorIJ testdata = new VectorIJ(1, 2);
    VectorIJ result = twoVarCon.setTo(testdata);
    assertSame(result, twoVarCon);
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
    assertEqualVector(new UnwritableVectorIJ(1, 2), testdata);
  }

  @Test
  public void testSetToDoubleUnwritableVectorIJ() {
    VectorIJ testdata = new VectorIJ(5, 7);
    VectorIJ result = twoVarCon.setTo(10, testdata);
    assertSame(result, twoVarCon);
    assertEqualVector(new UnwritableVectorIJ(50, 70), twoVarCon);
    assertEqualVector(new UnwritableVectorIJ(5, 7), testdata);
  }

  @Test
  public void testSetToDoubleDouble() {
    VectorIJ result = twoVarCon.setTo(21, 13);
    assertSame(result, twoVarCon);
    assertEqualVector(new UnwritableVectorIJ(21, 13), twoVarCon);
  }

  @Test
  public void testSetToDoubleArray() {
    VectorIJ result = twoVarCon.setTo(new double[] {1, 2});
    assertSame(result, twoVarCon);
    assertEqualVector(new UnwritableVectorIJ(1, 2), twoVarCon);
  }

  @Test
  public void testSetToIntDoubleArray() {
    VectorIJ result = twoVarCon.setTo(10, new double[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12});
    assertSame(result, twoVarCon);
    assertEqualVector(new UnwritableVectorIJ(10, 11), twoVarCon);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testSetToUnitizedZeroVectorException() {
    new VectorIJ().setToUnitized(VectorIJ.ZERO);
  }

  @Test
  public void testSetToUnitized() {
    VectorIJ testdata = new VectorIJ(3, 5);
    VectorIJ result = twoVarCon.setToUnitized(testdata);
    assertSame(result, twoVarCon);
    assertComponentEquals(
        new UnwritableVectorIJ(1.0 / Math.sqrt(9.0 + 25.0), new UnwritableVectorIJ(3, 5)),
        twoVarCon, TIGHT_TOLERANCE);
    assertEqualVector(new UnwritableVectorIJ(3, 5), testdata);
  }

  @Test
  public void testSetToNegated() {
    VectorIJ testdata = new VectorIJ(3, -5);
    VectorIJ result = twoVarCon.setToNegated(testdata);
    assertSame(result, twoVarCon);
    assertEquivalentVector(new UnwritableVectorIJ(-3, 5), twoVarCon);
    assertEquivalentVector(new UnwritableVectorIJ(3, -5), testdata);
  }

  @Test
  public void testAddNoBuffer() {
    VectorIJ c = VectorIJ.add(a, b);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertEqualVector(new VectorIJ(5, 7), c);
  }

  @Test
  public void testAdd() {
    VectorIJ d = VectorIJ.add(a, b, c);
    assertSame(c, d);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertEqualVector(new VectorIJ(5, 7), c);
  }

  @Test
  public void testAddOverA() {
    VectorIJ d = VectorIJ.add(a, b, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJ(5, 7), a);
    assertEqualVector(new VectorIJ(4, 5), b);
  }

  @Test
  public void testAddOverB() {
    VectorIJ d = VectorIJ.add(a, b, b);
    assertSame(d, b);
    assertEqualVector(new VectorIJ(5, 7), b);
    assertEqualVector(new VectorIJ(1, 2), a);
  }

  @Test
  public void testAddAAOverA() {
    VectorIJ d = VectorIJ.add(a, a, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJ(2, 4), a);
  }

  @Test
  public void testNewAdd() {
    VectorIJ d = VectorIJ.add(a, b);
    assertNotSame(a, d);
    assertNotSame(b, d);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertEqualVector(new VectorIJ(5, 7), d);
  }

  @Test
  public void testAddRSSNoBuffer() {
    VectorIJ c = VectorIJ.addRSS(a, b);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertComponentEquals(new VectorIJ(Math.sqrt(17), Math.sqrt(29)), c, TIGHT_TOLERANCE);
  }

  @Test
  public void testAddRSS() {
    VectorIJ d = VectorIJ.addRSS(a, b, c);
    assertSame(c, d);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertComponentEquals(new VectorIJ(Math.sqrt(17), Math.sqrt(29)), c, TIGHT_TOLERANCE);
  }

  @Test
  public void testAddRSSOverA() {
    VectorIJ d = VectorIJ.addRSS(a, b, a);
    assertSame(d, a);
    assertComponentEquals(new VectorIJ(Math.sqrt(17), Math.sqrt(29)), a, TIGHT_TOLERANCE);
    assertEqualVector(new VectorIJ(4, 5), b);
  }

  @Test
  public void testAddRSSOverB() {
    VectorIJ d = VectorIJ.addRSS(a, b, b);
    assertSame(d, b);
    assertComponentEquals(new VectorIJ(Math.sqrt(17), Math.sqrt(29)), b, TIGHT_TOLERANCE);
    assertEqualVector(new VectorIJ(1, 2), a);
  }

  @Test
  public void testAddRSSAAOverA() {
    VectorIJ d = VectorIJ.addRSS(a, a, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJ(Math.sqrt(2), Math.sqrt(8)), a);
  }

  @Test
  public void testNewAddRSS() {
    VectorIJ d = VectorIJ.addRSS(a, b);
    assertNotSame(a, d);
    assertNotSame(b, d);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertComponentEquals(new VectorIJ(Math.sqrt(17), Math.sqrt(29)), d, TIGHT_TOLERANCE);
  }

  @Test
  public void testAddAll() {
    VectorIJ d = VectorIJ.addAll(ImmutableList.of(a, b), c);
    assertSame(c, d);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertEqualVector(new VectorIJ(5, 7), c);
  }

  @Test
  public void testAddAllOverA() {
    VectorIJ d = VectorIJ.addAll(ImmutableList.of(a, b), a);
    assertSame(d, a);
    assertEqualVector(new VectorIJ(5, 7), a);
    assertEqualVector(new VectorIJ(4, 5), b);
  }

  @Test
  public void testAddAllOverB() {
    VectorIJ d = VectorIJ.addAll(ImmutableList.of(a, b), b);
    assertSame(d, b);
    assertEqualVector(new VectorIJ(5, 7), b);
    assertEqualVector(new VectorIJ(1, 2), a);
  }

  @Test
  public void testAddAllAAOverA() {
    VectorIJ d = VectorIJ.addAll(ImmutableList.of(a, a), a);
    assertSame(d, a);
    assertEqualVector(new VectorIJ(2, 4), a);
  }

  @Test
  public void testNewAddAll() {
    VectorIJ d = VectorIJ.addAll(ImmutableList.of(a, b));
    assertNotSame(a, d);
    assertNotSame(b, d);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertEqualVector(new VectorIJ(5, 7), d);
  }

  @Test
  public void testSubtractNoBuffer() {
    VectorIJ d = VectorIJ.subtract(a, c);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(1, -1), c);
    assertEqualVector(new VectorIJ(0, 3), d);
  }

  @Test
  public void testSubtract() {
    VectorIJ d = VectorIJ.subtract(a, c, b);
    assertSame(d, b);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(1, -1), c);
    assertEqualVector(new VectorIJ(0, 3), b);
  }

  @Test
  public void testSubtractOverA() {
    VectorIJ d = VectorIJ.subtract(a, c, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJ(1, -1), c);
    assertEqualVector(new VectorIJ(0, 3), a);
  }

  @Test
  public void testSubtractOverC() {
    VectorIJ d = VectorIJ.subtract(a, c, c);
    assertSame(d, c);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(0, 3), c);
  }

  @Test
  public void testSubtractAAOverA() {
    VectorIJ d = VectorIJ.subtract(a, a, a);
    assertSame(d, a);
    assertEqualVector(VectorIJ.ZERO, a);
  }

  @Test
  public void testNewSubtract() {
    VectorIJ d = VectorIJ.subtract(a, c);
    assertNotSame(d, a);
    assertNotSame(d, c);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(1, -1), c);
    assertEqualVector(new VectorIJ(0, 3), d);
  }

  @Test
  public void testCross() {
    VectorIJK d = VectorIJ.cross(a, c, bIJK);
    assertSame(d, bIJK);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(1, -1), c);
    assertEqualVector(new VectorIJK(0, 0, -3), bIJK);
  }

  @Test
  public void testNewCross() {
    VectorIJK d = VectorIJ.cross(a, c);
    assertNotSame(d, a);
    assertNotSame(d, c);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(1, -1), c);
    assertEqualVector(new VectorIJK(0, 0, -3), d);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testUCrossZeroLeftVectorException() {
    VectorIJ.uCross(VectorIJ.ZERO, a, bIJK);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testUCrossZeroRightVectorException() {
    VectorIJ.uCross(a, VectorIJ.ZERO, bIJK);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testUCrossZeroResultException() {
    VectorIJ.uCross(a, a, bIJK);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testUCrossAAOverA() {
    VectorIJ.uCross(a, a, bIJK);
  }

  @Test
  public void testUCross() {
    VectorIJK d = VectorIJ.uCross(a, c, bIJK);
    assertSame(d, bIJK);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(1, -1), c);
    assertEqualVector(new VectorIJK(1.0 / Math.sqrt(9.0), new VectorIJK(0, 0, -3)), bIJK);
  }

  @Test
  public void testNewUCross() {
    VectorIJK d = VectorIJ.uCross(a, c);
    assertNotSame(d, a);
    assertNotSame(d, c);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(1, -1), c);
    assertEqualVector(new VectorIJK(1.0 / Math.sqrt(9.0), new VectorIJK(0, 0, -3)), d);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJDoubleUnwritableVectorIJVectorIJ() {
    VectorIJ d = VectorIJ.combine(2.0, a, -3.0, b, c);
    assertSame(d, c);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertEqualVector(new VectorIJ(-10, -11), c);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJDoubleUnwritableVectorIJVectorIJOverA() {
    VectorIJ d = VectorIJ.combine(2.0, a, -3.0, b, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertEqualVector(new VectorIJ(-10, -11), a);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJDoubleUnwritableVectorIJVectorIJOverB() {
    VectorIJ d = VectorIJ.combine(2.0, a, -3.0, b, b);
    assertSame(d, b);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(-10, -11), b);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJDoubleUnwritableVectorIJVectorIJAAOverA() {
    VectorIJ d = VectorIJ.combine(2.0, a, -1.0, a, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJ(1, 2), a);
  }

  @Test
  public void testNewCombineDoubleUnwritableVectorIJDoubleUnwritableVectorIJVectorIJ() {
    VectorIJ d = VectorIJ.combine(2.0, a, -3.0, b);
    assertNotSame(d, a);
    assertNotSame(d, b);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertEqualVector(new VectorIJ(-10, -11), d);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJDoubleUnwritableVectorIJDoubleUnwritableVectorIJVectorIJ() {
    VectorIJ d = new VectorIJ();
    VectorIJ e = VectorIJ.combine(2.0, a, -3.0, b, 1.0, c, d);
    assertSame(e, d);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertEqualVector(new VectorIJ(1, -1), c);
    assertEqualVector(new VectorIJ(-9, -12), d);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJDoubleUnwritableVectorIJDoubleUnwritableVectorIJVectorIJOverA() {
    VectorIJ e = VectorIJ.combine(2.0, a, -3.0, b, 1.0, c, a);
    assertSame(e, a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertEqualVector(new VectorIJ(1, -1), c);
    assertEqualVector(new VectorIJ(-9, -12), a);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJDoubleUnwritableVectorIJDoubleUnwritableVectorIJVectorIJOverB() {
    VectorIJ e = VectorIJ.combine(2.0, a, -3.0, b, 1.0, c, b);
    assertSame(e, b);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(1, -1), c);
    assertEqualVector(new VectorIJ(-9, -12), b);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJDoubleUnwritableVectorIJDoubleUnwritableVectorIJVectorIJOverC() {
    VectorIJ e = VectorIJ.combine(2.0, a, -3.0, b, 1.0, c, c);
    assertSame(e, c);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertEqualVector(new VectorIJ(-9, -12), c);
  }

  @Test
  public void testCombineDoubleUnwritableVectorIJDoubleUnwritableVectorIJDoubleUnwritableVectorIJVectorIJAAOverA() {
    VectorIJ e = VectorIJ.combine(1.0, a, -3.0, a, 4.0, a, a);
    assertSame(e, a);
    assertEqualVector(new VectorIJ(2, 4), a);
  }

  @Test
  public void testNewCombineDoubleUnwritableVectorIJDoubleUnwritableVectorIJDoubleUnwritableVectorIJVectorIJ() {
    VectorIJ e = VectorIJ.combine(2.0, a, -3.0, b, 1.0, c);
    assertNotSame(e, a);
    assertNotSame(e, b);
    assertNotSame(e, c);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertEqualVector(new VectorIJ(1, -1), c);
    assertEqualVector(new VectorIJ(-9, -12), e);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testProjectZeroRightVectorException() {
    VectorIJ.project(a, VectorIJ.ZERO, b);
  }

  @Test
  public void testProjectZeroLeftVector() {
    VectorIJ d = VectorIJ.project(VectorIJ.ZERO, a, b);
    assertSame(d, b);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(VectorIJ.ZERO, b);
  }

  @Test
  public void testProject() {
    VectorIJ d = VectorIJ.project(a, b, c);
    assertSame(d, c);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertComponentEquals(new VectorIJ(14.0 / 41.0, b), c, TIGHT_TOLERANCE);
  }

  @Test
  public void testProjectOverA() {
    VectorIJ d = VectorIJ.project(a, b, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertComponentEquals(new VectorIJ(14.0 / 41.0, b), a, TIGHT_TOLERANCE);
  }

  @Test
  public void testProjectOverB() {
    c.setTo(b);
    VectorIJ d = VectorIJ.project(a, b, b);
    assertSame(d, b);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertComponentEquals(new VectorIJ(14.0 / 41.0, c), b, TIGHT_TOLERANCE);
  }

  @Test
  public void testProjectAAOverA() {
    VectorIJ d = VectorIJ.project(a, a, a);
    assertSame(d, a);
    assertEqualVector(new VectorIJ(1, 2), a);
  }

  @Test
  public void testNewProject() {
    VectorIJ d = VectorIJ.project(a, b);
    assertNotSame(d, a);
    assertNotSame(d, b);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertComponentEquals(new VectorIJ(14.0 / 41.0, b), d, TIGHT_TOLERANCE);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testLineProjectZeroNormalException() {
    VectorIJ.lineProject(a, VectorIJ.ZERO, b);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testLineProjectZeroNormalZeroVectorException() {
    VectorIJ.lineProject(VectorIJ.ZERO, VectorIJ.ZERO, a);
  }

  @Test
  public void testLineProjectZeroVector() {
    VectorIJ d = VectorIJ.lineProject(VectorIJ.ZERO, a, b);
    assertSame(d, b);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(0, 0), b);
  }

  @Test
  public void testLineProject() {
    VectorIJ d = VectorIJ.lineProject(a, b, c);
    VectorIJ normb = new VectorIJ(5, -4);
    assertSame(d, c);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertComponentEquals(VectorIJ.project(a, normb), c, TIGHT_TOLERANCE);
  }

  @Test
  public void testLineProjectOverA() {
    VectorIJ d = VectorIJ.lineProject(a, b, a);
    VectorIJ normb = new VectorIJ(5, -4);
    assertSame(d, a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertComponentEquals(VectorIJ.project(a, normb), a, TIGHT_TOLERANCE);
  }

  @Test
  public void testLineProjectOverB() {
    VectorIJ d = VectorIJ.lineProject(a, b, b);
    VectorIJ normb = new VectorIJ(5, -4);
    assertSame(d, b);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertComponentEquals(VectorIJ.project(a, normb), b, TIGHT_TOLERANCE);
  }

  @Test
  public void testLineProjectAAOverA() {
    VectorIJ d = VectorIJ.lineProject(a, a, a);
    assertSame(d, a);
    assertEqualVector(VectorIJ.ZERO, a);
  }

  @Test
  public void testNewLineProject() {
    VectorIJ d = VectorIJ.lineProject(a, b);
    VectorIJ normb = new VectorIJ(5, -4);
    assertNotSame(d, a);
    assertNotSame(d, b);
    assertEqualVector(new VectorIJ(1, 2), a);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertComponentEquals(VectorIJ.project(a, normb), d, TIGHT_TOLERANCE);
  }

  @Test
  public void testAsVectorIJK() {
    VectorIJK asijk = b.asVectorIJK(bIJK);
    assertEquals(VectorIJK.class, asijk.getClass());
    assertSame(asijk, bIJK);
    assertNotSame(asijk, b);
    assertEquivalentVector(new UnwritableVectorIJK(b.getI(), b.getJ(), 0), asijk);
  }

  @Test
  public void testAsNewVectorIJK() {
    VectorIJK d = b.asVectorIJK();
    assertNotSame(d, b);
    assertEqualVector(new VectorIJ(4, 5), b);
    assertEquivalentVector(new UnwritableVectorIJK(b.getI(), b.getJ(), 0), d);
  }

  static void checkStaticFinalMembers() {

    assertEqualVector(new UnwritableVectorIJ(0, 0), VectorIJ.ZERO);
    assertEqualVector(new UnwritableVectorIJ(1, 0), VectorIJ.I);
    assertEqualVector(new UnwritableVectorIJ(0, 1), VectorIJ.J);
    assertEqualVector(new UnwritableVectorIJ(-1, 0), VectorIJ.MINUS_I);
    assertEqualVector(new UnwritableVectorIJ(0, -1), VectorIJ.MINUS_J);

  }

}
