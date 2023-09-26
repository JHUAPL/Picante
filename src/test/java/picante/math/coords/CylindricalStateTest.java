package picante.math.coords;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import org.junit.Before;
import org.junit.Test;

public class CylindricalStateTest {

  private CylindricalState state;
  private CylindricalState newState;

  private CylindricalVector position;
  private CylindricalVector velocity;

  private CylindricalVector newPosition;
  private CylindricalVector newVelocity;

  @Before
  public void setUp() throws Exception {
    position = new CylindricalVector(102.0, 10.1, -234.0);
    velocity = new CylindricalVector(-123.0, 12.1, .13541);
    state = new CylindricalState(position, velocity);

    newPosition = new CylindricalVector(012.0, 3.1, 3.141592);
    newVelocity = new CylindricalVector(1234.2, 4.2012, 3132.01);
    newState = new CylindricalState(newPosition, newVelocity);
  }

  @Test
  public void testCylindricalState() {
    state = CylindricalState.ZERO;
    assertEquals(CylindricalVector.ZERO, state.getPosition());
    assertEquals(CylindricalVector.ZERO, state.getVelocity());
  }

  @Test
  public void testCylindricalStateUnwritableCylindricalCoordUnwritableCylindricalCoord() {
    state = new CylindricalState(position, velocity);
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
    assertEquals("UnwritableCylindricalState [position=" + state.getPosition() + ", velocity="
        + state.getVelocity() + "]", state.toString());
  }


}
