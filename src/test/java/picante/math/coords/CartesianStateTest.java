package picante.math.coords;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotSame;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

public class CartesianStateTest {

  private double x1;
  private double y1;
  private double z1;
  private double dx1;
  private double dy1;
  private double dz1;

  private double x2;
  private double y2;
  private double z2;
  private double dx2;
  private double dy2;
  private double dz2;

  private CartesianState state;
  private CartesianState newState;

  private UnwritableVectorIJK position;
  private UnwritableVectorIJK velocity;

  private UnwritableVectorIJK newPosition;
  private UnwritableVectorIJK newVelocity;

  @Before
  public void setUp() throws Exception {

    x1 = 102.0;
    y1 = 10.1;
    z1 = -234.0;
    dx1 = -123.0;
    dy1 = 12.1;
    dz1 = .13541;

    x2 = 012.0;
    y2 = 3.1;
    z2 = 3.141592;
    dx2 = 1234.2;
    dy2 = 4.2012;
    dz2 = 3132.01;

    position = new VectorIJK(x1, y1, z1);
    velocity = new VectorIJK(dx1, dy1, dz1);
    state = new CartesianState(position, velocity);

    newPosition = new VectorIJK(x2, y2, z2);
    newVelocity = new VectorIJK(dx2, dy2, dz2);
    newState = new CartesianState(newPosition, newVelocity);
  }

  @Test
  public void testCartesianState() {
    state = CartesianState.ZERO;
    assertEquals(VectorIJK.ZERO, state.getPosition());
    assertEquals(VectorIJK.ZERO, state.getVelocity());
  }

  @Test
  public void testCartesianStateUnwritableVectorIJKUnwritableVectorIJK() {
    state = new CartesianState(position, velocity);
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

    assertEquals(state, state);
    assertEquals(newState, newState);
    assertNotSame(state, newState);
    assertNotEquals(state, newState);

    state = new CartesianState(position, velocity);
    newState = new CartesianState(position, velocity);
    assertNotSame(state, newState);
    assertEquals(state, newState);

    state = new CartesianState(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    newState = new CartesianState(new VectorIJK(x1 + 0.1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    assertNotSame(state, newState);
    assertNotEquals(state, newState);

    state = new CartesianState(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    newState = new CartesianState(new VectorIJK(x1, y1 + 0.1, z1), new VectorIJK(dx1, dy1, dz1));
    assertNotSame(state, newState);
    assertNotEquals(state, newState);

    state = new CartesianState(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    newState = new CartesianState(new VectorIJK(x1, y1, z1 + 0.1), new VectorIJK(dx1, dy1, dz1));
    assertNotSame(state, newState);
    assertNotEquals(state, newState);

    state = new CartesianState(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    newState = new CartesianState(new VectorIJK(x1, y1, z1), new VectorIJK(dx1 + 0.1, dy1, dz1));
    assertNotSame(state, newState);
    assertNotEquals(state, newState);

    state = new CartesianState(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    newState = new CartesianState(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1 + 0.1, dz1));
    assertNotSame(state, newState);
    assertNotEquals(state, newState);

    state = new CartesianState(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    newState = new CartesianState(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1 + 0.1));
    assertNotSame(state, newState);
    assertNotEquals(state, newState);

    state = new CartesianState(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    newState = null;
    assertNotSame(state, newState);
    assertNotEquals(state, newState);

    state = new CartesianState(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    assertNotEquals(state, new Object());

  }

  @Test
  public void testToString() {
    assertEquals(
        "[" + state.getPosition().getI() + "," + state.getPosition().getJ() + ","
            + state.getPosition().getK() + "; " + state.getVelocity().getI() + ","
            + state.getVelocity().getJ() + "," + state.getVelocity().getK() + "]",
        state.toString());
    assertEquals(
        "[" + newState.getPosition().getI() + "," + newState.getPosition().getJ() + ","
            + newState.getPosition().getK() + "; " + newState.getVelocity().getI() + ","
            + newState.getVelocity().getJ() + "," + newState.getVelocity().getK() + "]",
        newState.toString());
  }

}
