package picante.mechanics;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static picante.junit.AssertTools.assertEqualStateVector;
import static picante.junit.AssertTools.assertEqualVector;
import static picante.junit.AssertTools.assertEquivalentDouble;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

public class UnwritableStateVectorTest {

  private static final double TOLERANCE = 1e-15;

  private UnwritableStateVector state;
  private UnwritableStateVector copyCon;
  private UnwritableStateVector otherVelocity;
  private UnwritableStateVector otherPosition;
  private VectorIJK p;
  private VectorIJK v;

  @Before
  public void setUp() throws Exception {

    p = new VectorIJK(VectorIJK.I);
    v = new VectorIJK(VectorIJK.J);
    state = new UnwritableStateVector(p, v);
    copyCon = new UnwritableStateVector(state);
    otherVelocity = new UnwritableStateVector(VectorIJK.I, VectorIJK.K);
    otherPosition = new UnwritableStateVector(VectorIJK.J, VectorIJK.J);

  }

  @After
  public void tearDown() throws Exception {
    StateVectorTest.checkStaticFinalMembers();
  }

  @Test
  public void testUnwritableStateVectorUnwritableVectorIJKUnwritableVectorIJK() {
    assertEqualVector(p, state.position);
    assertNotSame(p, state.position);
    assertEqualVector(v, state.velocity);
    assertNotSame(v, state.velocity);
  }

  @Test
  public void testUnwritableStateVectorUnwritableStateVector() {
    state = new UnwritableStateVector(StateVector.ZERO);
    assertNotSame(StateVector.ZERO.position, state.position);
    assertNotSame(StateVector.ZERO.velocity, state.velocity);
    assertEqualStateVector(new UnwritableStateVector(VectorIJK.ZERO, VectorIJK.ZERO),
        StateVector.ZERO);
    assertEqualStateVector(StateVector.ZERO, state);
  }

  @Test
  public void testUnwritableStateVectorDoubleDoubleDoubleDoubleDoubleDouble() {
    state = new UnwritableStateVector(1, 2, 3, 4, 5, 6);
    assertEqualStateVector(new StateVector(new VectorIJK(1, 2, 3), new VectorIJK(4, 5, 6)), state);
  }

  @Test
  public void testGetPosition() {
    UnwritableVectorIJK result = state.getPosition();

    /*
     * This test is questionable, as the API does not clearly indicate that this is the case;
     * however it is precisely how the current implementation functions.
     */
    assertSame(result, state.position);

    assertEqualVector(VectorIJK.I, result);
  }

  @Test
  public void testGetVelocity() {
    UnwritableVectorIJK result = state.getVelocity();

    /*
     * This test is questionable, as the API does not clearly indicate that this is the case;
     * however it is precisely how the current implementation functions.
     */
    assertSame(result, state.velocity);

    assertEqualVector(VectorIJK.J, result);
  }

  @Test
  public void testCopyOf() {

    UnwritableStateVector unwritable = new UnwritableStateVector(1, 2, 3, 4, 5, 6);
    UnwritableStateVector notUnwritable = new UnwritableStateVector(1, 2, 3, 7, 8, 9) {};

    UnwritableStateVector result = UnwritableStateVector.copyOf(unwritable);
    assertSame(result, unwritable);

    result = UnwritableStateVector.copyOf(notUnwritable);
    assertNotSame(result, notUnwritable);
    assertEquals(result, notUnwritable);
    assertEquals(UnwritableStateVector.class, result.getClass());

  }

  @Test
  public void testCreateNegated() {
    UnwritableStateVector state = new UnwritableStateVector(1, 2, 3, 4, 5, 6);
    UnwritableStateVector negated = state.createNegated();
    assertEqualStateVector(new UnwritableStateVector(-1, -2, -3, -4, -5, -6), negated);
  }

  @Test
  public void testCreateUnitized() {
    UnwritableStateVector state = new UnwritableStateVector(1, 2, 3, 4, 5, 6);
    UnwritableStateVector unitized = state.createUnitized();
    assertEqualStateVector(new StateVector(1, 2, 3, 4, 5, 6).unitize(), unitized);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateUnitizedZeroVectorException() {
    new UnwritableStateVector(StateVector.ZERO).createUnitized();

  }

  @Test
  public void testGetLength() {
    assertEquivalentDouble(new VectorIJK(1, 2, 3).getLength(),
        new UnwritableStateVector(1, 2, 3, 4, 5, 6).getLength());
  }

  @Test
  public void testGetSeparation() {
    assertEquivalentDouble(new VectorIJK(1, 2, 3).getSeparation(new VectorIJK(-1, 0, 1)),
        new UnwritableStateVector(1, 2, 3, 4, 5, 6)
            .getSeparation(new UnwritableStateVector(-1, 0, 1, 3, 8, 9)));
  }

  @Test
  public void testGetDot() {
    assertEquivalentDouble(new VectorIJK(1, 2, 3).getDot(new VectorIJK(1, 0, -1)),
        new UnwritableStateVector(1, 2, 3, 4, 5, 6)
            .getDot(new UnwritableStateVector(1, 0, -1, 3, 8, 9)));
  }

  @Test
  public void testGetLengthDerivative() {
    assertEquals(8.5523597411975807,
        new UnwritableStateVector(1, 2, 3, 4, 5, 6).getLengthDerivative(), TOLERANCE);
  }

  @Test
  public void testGetSeparationDerivative() {
    assertEquals(0.64164534697691755, new UnwritableStateVector(1, 2, 3, 4, 5, 6)
        .getSeparationDerivative(new UnwritableStateVector(1, -1, 2, 5, -8, 13)), TOLERANCE);
  }

  @Test
  public void testGetDotDerivative() {
    assertEquals(39, new UnwritableStateVector(1, 2, 3, 4, 5, 6)
        .getDotDerivative(new UnwritableStateVector(1, -1, 2, 5, -8, 13)), TOLERANCE);
  }

  @Test
  public void testEqualsObject() {

    assertTrue(state.equals(copyCon));
    assertNotSame(state, copyCon);
    assertEqualStateVector(new UnwritableStateVector(VectorIJK.I, VectorIJK.J), state);
    assertEqualStateVector(new UnwritableStateVector(VectorIJK.I, VectorIJK.J), copyCon);

    assertTrue(state.equals(state));
    assertEqualStateVector(new UnwritableStateVector(VectorIJK.I, VectorIJK.J), state);

    assertFalse(state.equals(null));
    assertEqualStateVector(new UnwritableStateVector(VectorIJK.I, VectorIJK.J), state);

    assertFalse(state.equals(""));
    assertEqualStateVector(new UnwritableStateVector(VectorIJK.I, VectorIJK.J), state);

    assertFalse(state.equals(otherVelocity));
    assertEqualStateVector(new UnwritableStateVector(VectorIJK.I, VectorIJK.J), state);
    assertEqualStateVector(new UnwritableStateVector(VectorIJK.I, VectorIJK.K), otherVelocity);

    assertFalse(state.equals(otherPosition));
    assertEqualStateVector(new UnwritableStateVector(VectorIJK.I, VectorIJK.J), state);
    assertEqualStateVector(new UnwritableStateVector(VectorIJK.J, VectorIJK.J), otherPosition);

    UnwritableStateVector subClass = new UnwritableStateVector(VectorIJK.I, VectorIJK.J) {};

    assertTrue(subClass.equals(state));
    assertEqualStateVector(new UnwritableStateVector(VectorIJK.I, VectorIJK.J), state);
    assertEqualStateVector(new UnwritableStateVector(VectorIJK.I, VectorIJK.J), subClass);

    assertTrue(state.equals(subClass));
    assertEqualStateVector(new UnwritableStateVector(VectorIJK.I, VectorIJK.J), state);
    assertEqualStateVector(new UnwritableStateVector(VectorIJK.I, VectorIJK.J), subClass);

  }

  @Test
  public void testHashCode() {

    /*
     * Simply check that state vectors that are equal, but are different instances have equal
     * hashcodes.
     */
    assertEquals(state.hashCode(), copyCon.hashCode());
    assertNotSame(state, copyCon);

  }

}
