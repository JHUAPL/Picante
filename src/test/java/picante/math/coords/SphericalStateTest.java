package picante.math.coords;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import org.junit.Before;
import org.junit.Test;

public class SphericalStateTest {

  private SphericalState state;
  private SphericalState newState;

  private SphericalVector position;
  private SphericalVector velocity;

  private SphericalVector newPosition;
  private SphericalVector newVelocity;

  @Before
  public void setUp() throws Exception {
    position = new SphericalVector(102.0, 10.1, -234.0);
    velocity = new SphericalVector(-123.0, 12.1, .13541);
    state = new SphericalState(position, velocity);

    newPosition = new SphericalVector(012.0, 3.1, 3.141592);
    newVelocity = new SphericalVector(1234.2, 4.2012, 3132.01);
    newState = new SphericalState(newPosition, newVelocity);
  }

  @Test
  public void testSphericalState() {
    state = SphericalState.ZERO;
    assertEquals(SphericalVector.ZERO, state.getPosition());
    assertEquals(SphericalVector.ZERO, state.getVelocity());
  }

  @Test
  public void testSphericalStateUnwritableSphericalCoordUnwritableSphericalCoord() {
    state = new SphericalState(position, velocity);
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
        "[" + state.getPosition().getRadius() + "," + state.getPosition().getColatitude() + ","
            + state.getPosition().getLongitude() + "; " + state.getVelocity().getRadius() + ","
            + state.getVelocity().getColatitude() + "," + state.getVelocity().getLongitude() + "]",
        state.toString());
  }

}
