package picante.mechanics;

import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEqualStateVector;
import static picante.junit.AssertTools.assertEqualVector;
import static picante.junit.AssertTools.assertEquivalentStateVector;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.ImmutableList;
import picante.math.vectorspace.VectorIJK;

public class StateVectorTest {

  private static final double TOLERANCE = 1e-15;

  private StateVector state;
  private VectorIJK p;
  private VectorIJK v;

  private StateVector a;
  private StateVector b;
  private StateVector c;
  private StateVector d;

  private final UnwritableStateVector srcA =
      new UnwritableStateVector(new VectorIJK(1, 2, 3), new VectorIJK(4, 5, 6));
  private final UnwritableStateVector srcB =
      new UnwritableStateVector(new VectorIJK(1, -1, 2), new VectorIJK(5, -8, 13));
  private final UnwritableStateVector srcC =
      new UnwritableStateVector(new VectorIJK(2, -2, 4), new VectorIJK(3, -3, 9));
  private final UnwritableStateVector srcD = new UnwritableStateVector(0.1, 1e10, 1e12, 1, 1, 1);

  @Before
  public void setUp() throws Exception {
    p = new VectorIJK(VectorIJK.MINUS_I);
    v = new VectorIJK(VectorIJK.K);
    state = new StateVector(p, v);

    a = new StateVector(srcA);
    b = new StateVector(srcB);
    c = new StateVector(srcC);
    d = new StateVector(srcD);
  }

  @After
  public void tearDown() throws Exception {
    checkStaticFinalMembers();
  }

  @Test
  public void testStateVector() {
    assertEqualStateVector(StateVector.ZERO, new StateVector());
  }

  @Test
  public void testStateVectorUnwritableVectorIJKUnwritableVectorIJK() {
    assertEqualVector(p, state.position);
    assertNotSame(p, state.position);
    assertEqualVector(v, state.velocity);
    assertNotSame(v, state.velocity);
  }

  @Test
  public void testStateVectorUnwritableStateVector() {
    state = new StateVector(StateVector.ZERO);
    assertNotSame(StateVector.ZERO.position, state.position);
    assertNotSame(StateVector.ZERO.velocity, state.velocity);
    assertEqualStateVector(new UnwritableStateVector(VectorIJK.ZERO, VectorIJK.ZERO),
        StateVector.ZERO);
    assertEqualStateVector(StateVector.ZERO, state);
  }

  @Test
  public void testStateVectorDoubleDoubleDoubleDoubleDoubleDouble() {
    state = new StateVector(1, 2, 3, 4, 5, 6);
    assertEqualStateVector(a, state);
  }

  @Test
  public void testCreateNegated() {
    StateVector state = new StateVector(1, 2, 3, 4, 5, 6);
    StateVector negated = state.createNegated();
    assertEqualStateVector(new StateVector(-1, -2, -3, -4, -5, -6), negated);
  }

  @Test
  public void testCreateUnitized() {
    StateVector state = new StateVector(1, 2, 3, 4, 5, 6);
    StateVector unitized = state.createUnitized();
    assertEqualStateVector(new StateVector(1, 2, 3, 4, 5, 6).unitize(), unitized);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateUnitizedZeroVectorException() {
    new StateVector(StateVector.ZERO).createUnitized();

  }

  @Test
  public void testGetPosition() {
    VectorIJK result = state.getPosition();

    assertEqualVector(VectorIJK.MINUS_I, result);

    /*
     * Now change result, and verify that the contents of state's internals are synchronized with
     * the change.
     */
    result.setTo(VectorIJK.J);
    VectorIJK newResult = state.getPosition();
    assertEqualVector(VectorIJK.J, newResult);
  }

  @Test
  public void testGetVelocity() {
    VectorIJK result = state.getVelocity();

    assertEqualVector(VectorIJK.K, result);

    /*
     * Now change result, and verify that the contents of state's internals are synchronized with
     * the change.
     */
    result.setTo(VectorIJK.J);
    VectorIJK newResult = state.getVelocity();
    assertEqualVector(VectorIJK.J, newResult);
  }

  @Test
  public void testSetPosition() {
    state.setPosition(VectorIJK.J);
    assertNotSame(state.position, VectorIJK.J);
    assertEqualVector(VectorIJK.J, state.position);
    assertEqualVector(VectorIJK.K, state.velocity);
  }

  @Test
  public void testSetVelocity() {
    state.setVelocity(VectorIJK.MINUS_J);
    assertNotSame(state.velocity, VectorIJK.MINUS_J);
    assertEqualVector(VectorIJK.MINUS_J, state.velocity);
    assertEqualVector(VectorIJK.MINUS_I, state.position);
  }

  @Test
  public void testSetTo() {
    StateVector result = state.setTo(StateVector.ZERO);
    assertSame(result, state);
    assertNotSame(state.position, StateVector.ZERO.position);
    assertNotSame(state.velocity, StateVector.ZERO.velocity);
    assertEqualStateVector(StateVector.ZERO, state);
  }

  @Test
  public void testClear() {
    StateVector result = state.clear();
    assertSame(result, state);
    assertEqualStateVector(StateVector.ZERO, state);
    assertNotSame(StateVector.ZERO.position, state.position);
    assertNotSame(StateVector.ZERO.velocity, state.velocity);
  }

  @Test
  public void testNegate() {
    StateVector result = state.negate();
    assertSame(result, state);
    assertEqualStateVector(new UnwritableStateVector(p.createNegated(), v.createNegated()), result);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testUnitizeZeroPositionException() {
    new StateVector(StateVector.ZERO).unitize();
  }

  @Test
  public void testUnitize() {
    StateVector result = a.unitize();
    assertSame(result, a);
    assertEquivalentStateVector(new StateVector(0.26726124191242440, 0.53452248382484879,
        0.80178372573727330, 0.45816212899272751, 0.11454053224818202, -0.22908106449636367), a);

    result = b.unitize();
    assertSame(result, b);
    assertEquivalentStateVector(new StateVector(0.40824829046386307, -0.40824829046386307,
        0.81649658092772615, -0.61237243569579447, -0.61237243569579480, 0.0000000000000000), b);

    result = c.unitize();
    assertSame(result, c);
    assertEquivalentStateVector(new StateVector(0.40824829046386307, -0.40824829046386307,
        0.81649658092772615, -0.20412414523193154, 0.20412414523193154, 0.20412414523193162), c);

    result = d.unitize();
    assertSame(result, d);
    assertEquivalentStateVector(
        new StateVector(9.99950003749687689E-014, 9.99950003749687684E-003, 0.99995000374968768,
            9.99950003749586690E-013, 9.89851518560333843E-013, -9.89851518570335217E-015),
        d);

  }

  @Test
  public void testAdd() {
    StateVector d = StateVector.add(a, b, c);
    assertSame(c, d);
    assertEqualStateVector(srcA, a);
    assertEqualStateVector(srcB, b);
    assertEqualStateVector(new StateVector(new VectorIJK(2, 1, 5), new VectorIJK(9, -3, 19)), c);
  }

  @Test
  public void testAddOverA() {
    StateVector d = StateVector.add(a, b, a);
    assertSame(d, a);
    assertEqualStateVector(srcB, b);
    assertEqualStateVector(new StateVector(new VectorIJK(2, 1, 5), new VectorIJK(9, -3, 19)), a);
  }

  @Test
  public void testAddOverB() {
    StateVector d = StateVector.add(a, b, b);
    assertSame(d, b);
    assertEqualStateVector(srcA, a);
    assertEqualStateVector(new StateVector(new VectorIJK(2, 1, 5), new VectorIJK(9, -3, 19)), b);
  }

  @Test
  public void testAddAAOverA() {
    StateVector d = StateVector.add(a, a, a);
    assertSame(d, a);
    assertEqualStateVector(new StateVector(new VectorIJK(2, 4, 6), new VectorIJK(8, 10, 12)), a);
  }

  @Test
  public void testAddNoBuffer() {
    StateVector result = StateVector.add(a, b);
    assertNotSame(result, a);
    assertNotSame(result, b);
    assertEqualStateVector(new StateVector(2, 1, 5, 9, -3, 19), result);

    result = StateVector.add(a, a);
    assertNotSame(result, a);
    assertEqualStateVector(new StateVector(2, 4, 6, 8, 10, 12), result);
  }

  @Test
  public void testAddAll() {
    StateVector d = StateVector.addAll(ImmutableList.of(a, b), c);
    assertSame(c, d);
    assertEqualStateVector(new StateVector(1, 2, 3, 4, 5, 6), a);
    assertEqualStateVector(new StateVector(1, -1, 2, 5, -8, 13), b);
    assertEqualStateVector(new StateVector(2, 1, 5, 9, -3, 19), c);
  }

  @Test
  public void testAddAllOverA() {
    StateVector d = StateVector.addAll(ImmutableList.of(a, b), a);
    assertSame(d, a);
    assertEqualStateVector(new StateVector(2, 1, 5, 9, -3, 19), a);
    assertEqualStateVector(new StateVector(1, -1, 2, 5, -8, 13), b);
  }

  @Test
  public void testAddAllOverB() {
    StateVector d = StateVector.addAll(ImmutableList.of(a, b), b);
    assertSame(d, b);
    assertEqualStateVector(new StateVector(2, 1, 5, 9, -3, 19), b);
    assertEqualStateVector(new StateVector(1, 2, 3, 4, 5, 6), a);
  }

  @Test
  public void testAddAllAAOverA() {
    StateVector d = StateVector.addAll(ImmutableList.of(a, a), a);
    assertSame(d, a);
    assertEqualStateVector(new StateVector(2, 4, 6, 8, 10, 12), a);
  }

  @Test
  public void testNewAddAll() {
    StateVector d = StateVector.addAll(ImmutableList.of(a, b));
    assertNotSame(a, d);
    assertNotSame(b, d);
    assertEqualStateVector(new StateVector(1, 2, 3, 4, 5, 6), a);
    assertEqualStateVector(new StateVector(1, -1, 2, 5, -8, 13), b);
    assertEqualStateVector(new StateVector(2, 1, 5, 9, -3, 19), d);
  }

  @Test
  public void testSubtract() {
    StateVector d = StateVector.subtract(a, b, c);
    assertSame(d, c);
    assertEqualStateVector(srcA, a);
    assertEqualStateVector(srcB, b);
    assertEqualStateVector(new StateVector(new VectorIJK(0, 3, 1), new VectorIJK(-1, 13, -7)), c);
  }

  @Test
  public void testSubtractOverA() {
    StateVector d = StateVector.subtract(a, b, a);
    assertSame(d, a);
    assertEqualStateVector(srcB, b);
    assertEqualStateVector(new StateVector(new VectorIJK(0, 3, 1), new VectorIJK(-1, 13, -7)), a);
  }

  @Test
  public void testSubtractOverB() {
    StateVector d = StateVector.subtract(a, b, b);
    assertSame(d, b);
    assertEqualStateVector(srcA, a);
    assertEqualStateVector(new StateVector(new VectorIJK(0, 3, 1), new VectorIJK(-1, 13, -7)), b);
  }

  @Test
  public void testSubtractAAOverA() {
    StateVector d = StateVector.subtract(a, a, a);
    assertSame(d, a);
    assertEqualStateVector(StateVector.ZERO, a);
  }

  @Test
  public void testSubtractNoBuffer() {
    StateVector result = StateVector.subtract(a, b);
    assertNotSame(result, a);
    assertNotSame(result, b);
    assertEqualStateVector(new StateVector(0, 3, 1, -1, 13, -7), result);

    result = StateVector.subtract(a, a);
    assertNotSame(result, a);
    assertNotSame(result, StateVector.ZERO);
    assertEqualStateVector(StateVector.ZERO, result);
  }

  @Test
  public void testCross() {
    StateVector d = StateVector.cross(a, b, c);
    assertSame(d, c);
    assertEqualStateVector(srcA, a);
    assertEqualStateVector(srcB, b);
    assertEqualStateVector(new StateVector(7, 1, -3, 66, 0, -27), d);
  }

  @Test
  public void testCrossOverA() {
    StateVector d = StateVector.cross(a, b, a);
    assertSame(d, a);
    assertEqualStateVector(srcB, b);
    assertEqualStateVector(new StateVector(7, 1, -3, 66, 0, -27), d);
  }

  @Test
  public void testCrossOverB() {
    StateVector d = StateVector.cross(a, b, b);
    assertSame(d, b);
    assertEqualStateVector(srcA, a);
    assertEqualStateVector(new StateVector(7, 1, -3, 66, 0, -27), d);
  }

  @Test
  public void testCrossAAOverA() {
    StateVector d = StateVector.cross(a, a, a);
    assertSame(d, a);
    assertEqualStateVector(StateVector.ZERO, a);
  }

  @Test
  public void testCrossNoBuffer() {
    StateVector d = StateVector.cross(a, b);
    assertNotSame(d, a);
    assertNotSame(d, b);
    assertEqualStateVector(new StateVector(7, 1, -3, 66, 0, -27), d);

    d = StateVector.cross(a, a);
    assertNotSame(d, a);
    assertNotSame(d, StateVector.ZERO);
    assertEqualStateVector(StateVector.ZERO, d);
  }

  @Test
  public void testUCross() {
    StateVector d = StateVector.uCross(a, b, c);
    assertSame(d, c);
    assertEqualStateVector(srcA, a);
    assertEqualStateVector(srcB, b);
    assertComponentEquals(new StateVector(0.91132237686576700, 0.13018891098082386,
        -0.39056673294247157, 0.20521302917316309, -1.1981792993658873, 7.94373016154174272E-2), d,
        TOLERANCE);
  }

  @Test
  public void testUCrossOverA() {
    StateVector d = StateVector.uCross(a, b, a);
    assertSame(d, a);
    assertEqualStateVector(srcB, b);
    assertComponentEquals(new StateVector(0.91132237686576700, 0.13018891098082386,
        -0.39056673294247157, 0.20521302917316309, -1.1981792993658873, 7.94373016154174272E-2), d,
        TOLERANCE);
  }

  @Test
  public void testUCrossOverB() {
    StateVector d = StateVector.uCross(a, b, b);
    assertSame(d, b);
    assertEqualStateVector(srcA, a);
    assertComponentEquals(new StateVector(0.91132237686576700, 0.13018891098082386,
        -0.39056673294247157, 0.20521302917316309, -1.1981792993658873, 7.94373016154174272E-2), d,
        TOLERANCE);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testUCrossAAOverA() {
    /*
     * The cross product of any vector with itself is always going to be zero, which can not be
     * unitized.
     */
    StateVector.uCross(a, a, a);
  }

  @Test
  public void testUCrossNoBuffer() {
    StateVector d = StateVector.uCross(a, b);
    assertNotSame(d, a);
    assertNotSame(d, b);
    assertComponentEquals(new StateVector(0.91132237686576700, 0.13018891098082386,
        -0.39056673294247157, 0.20521302917316309, -1.1981792993658873, 7.94373016154174272E-2), d,
        TOLERANCE);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testUCrossNoBufferException() {
    StateVector.uCross(a, a);
  }

  @Test
  public void testCombineBuffer() {
    StateVector buffer = new StateVector();
    StateVector result = StateVector.combine(0.5, srcA, 2.0, srcB, buffer);

    assertSame(buffer, result);
    StateVector expected = new StateVector(2.5, -1, 5.5, 12, -13.5, 29.0);
    assertEqualStateVector(expected, result);
  }

  @Test
  public void testCombineNoBuffer() {
    StateVector result = StateVector.combine(0.5, srcA, 2.0, srcB);
    StateVector expected = new StateVector(2.5, -1, 5.5, 12, -13.5, 29.0);
    assertEqualStateVector(expected, result);
  }


  public static void checkStaticFinalMembers() {
    assertEqualStateVector(new UnwritableStateVector(VectorIJK.ZERO, VectorIJK.ZERO),
        StateVector.ZERO);
  }
}
