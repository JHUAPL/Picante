package picante.math.coords;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.UnwritableVectorIJ;
import picante.math.vectorspace.VectorIJ;

public class CartesianStateIJTest {

  private CartesianStateIJ state;
  private CartesianStateIJ newState;

  private UnwritableVectorIJ position;
  private UnwritableVectorIJ velocity;

  private UnwritableVectorIJ newPosition;
  private UnwritableVectorIJ newVelocity;

  @Before
  public void setUp() throws Exception {
    position = new VectorIJ(102.0, 10.1);
    velocity = new VectorIJ(-123.0, 12.1);
    state = new CartesianStateIJ(position, velocity);

    newPosition = new VectorIJ(012.0, 3.1);
    newVelocity = new VectorIJ(1234.2, 4.2012);
    newState = new CartesianStateIJ(newPosition, newVelocity);
  }

  @Test
  public void testCartesianStateIJ() {
    state = CartesianStateIJ.ZERO;
    assertEquals(VectorIJ.ZERO, state.getPosition());
    assertEquals(VectorIJ.ZERO, state.getVelocity());
  }

  @Test
  public void testCartesianStateIJUnwritableVectorIJUnwritableVectorIJ() {
    state = new CartesianStateIJ(position, velocity);
    assertEquals(position, state.getPosition());
    assertEquals(velocity, state.getVelocity());
  }

  @Test
  public void testGetPosition() {
    assertEquals(position, state.getPosition());
    assertEquals(newPosition, newState.getPosition());
  }

  @Test
  public void testGetVelocity() {
    assertEquals(velocity, state.getVelocity());
    assertEquals(newVelocity, newState.getVelocity());
  }

  @Test
  public void testHashCode() {
    assertNotSame(newState.hashCode(), state.hashCode());
  }

  @Test
  public void testEqualsObject() {
    assertNotSame(newState, state);
  }

  @Test
  public void testToString() {
    assertEquals(
        "[" + state.getPosition().getI() + "," + state.getPosition().getJ() + "; "
            + state.getVelocity().getI() + "," + state.getVelocity().getJ() + "]",
        state.toString());
    assertEquals(
        "[" + newState.getPosition().getI() + "," + newState.getPosition().getJ() + "; "
            + newState.getVelocity().getI() + "," + newState.getVelocity().getJ() + "]",
        newState.toString());
  }

}
