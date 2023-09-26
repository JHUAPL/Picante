package picante.math.coords;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import org.junit.Before;
import org.junit.Test;

public class PolarStateTest {

  private PolarState state;
  private PolarState newState;

  private PolarVector position;
  private PolarVector velocity;

  private PolarVector newPosition;
  private PolarVector newVelocity;

  @Before
  public void setUp() throws Exception {
    position = new PolarVector(102.0, 10.1);
    velocity = new PolarVector(-123.0, 12.1);
    state = new PolarState(position, velocity);

    newPosition = new PolarVector(012.0, 3.1);
    newVelocity = new PolarVector(1234.2, 4.2012);
    newState = new PolarState(newPosition, newVelocity);
  }

  @Test
  public void testPolarState() {
    state = PolarState.ZERO;
    assertEquals(PolarVector.ZERO, state.getPosition());
    assertEquals(PolarVector.ZERO, state.getVelocity());
  }

  @Test
  public void testPolarStateUnwritablePolarCoordUnwritablePolarCoord() {
    state = new PolarState(position, velocity);
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
        "[" + state.getPosition().getRadius() + "," + state.getPosition().getAngle() + "; "
            + state.getVelocity().getRadius() + "," + state.getVelocity().getAngle() + "]",
        state.toString());
  }

}
