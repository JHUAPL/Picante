package picante.math.coords;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import org.junit.Before;
import org.junit.Test;

public class LatitudinalStateTest {

  private LatitudinalState state;
  private LatitudinalState newState;

  private LatitudinalVector position;
  private LatitudinalVector velocity;

  private LatitudinalVector newPosition;
  private LatitudinalVector newVelocity;

  @Before
  public void setUp() throws Exception {
    position = new LatitudinalVector(102.0, 10.1, -234.0);
    velocity = new LatitudinalVector(-123.0, 12.1, .13541);
    state = new LatitudinalState(position, velocity);

    newPosition = new LatitudinalVector(012.0, 3.1, 3.141592);
    newVelocity = new LatitudinalVector(1234.2, 4.2012, 3132.01);
    newState = new LatitudinalState(newPosition, newVelocity);
  }

  @Test
  public void testLatitudinalState() {
    state = LatitudinalState.ZERO;
    assertEquals(LatitudinalVector.ZERO, state.getPosition());
    assertEquals(LatitudinalVector.ZERO, state.getVelocity());
  }

  @Test
  public void testLatitudinalStateUnwritableLatitudinalCoordUnwritableLatitudinalCoord() {
    state = new LatitudinalState(position, velocity);
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
        "[" + state.getPosition().getRadius() + "," + state.getPosition().getLatitude() + ","
            + state.getPosition().getLongitude() + "; " + state.getVelocity().getRadius() + ","
            + state.getVelocity().getLatitude() + "," + state.getVelocity().getLongitude() + "]",
        state.toString());
  }

}
