package picante.math.coords;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotSame;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

public class CartesianVectorFieldValueTest {

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

  private CartesianVectorFieldValue state;
  private CartesianVectorFieldValue newVectorFieldValue;

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
    state = new CartesianVectorFieldValue(position, velocity);

    newPosition = new VectorIJK(x2, y2, z2);
    newVelocity = new VectorIJK(dx2, dy2, dz2);
    newVectorFieldValue = new CartesianVectorFieldValue(newPosition, newVelocity);
  }

  @Test
  public void testCartesianVectorFieldValue() {
    state = new CartesianVectorFieldValue(VectorIJK.ZERO, VectorIJK.ZERO);
    assertEquals(VectorIJK.ZERO, state.getPosition());
    assertEquals(VectorIJK.ZERO, state.getValue());
  }

  @Test
  public void testCartesianVectorFieldValueUnwritableVectorIJKUnwritableVectorIJK() {
    state = new CartesianVectorFieldValue(position, velocity);
    assertEquals(position, state.getPosition());
    assertEquals(velocity, state.getValue());
  }

  @Test
  public void testGetPosition() {
    assertEquals(position, state.getPosition());
    assertEquals(newPosition, newVectorFieldValue.getPosition());
  }

  @Test
  public void testGetVelocity() {
    assertEquals(velocity, state.getValue());
    assertEquals(newVelocity, newVectorFieldValue.getValue());
  }

  @Test
  public void testHashCode() {
    assertNotSame(newVectorFieldValue.hashCode(), state.hashCode());
  }

  @Test
  public void testEqualsObject() {
    assertNotSame(newVectorFieldValue, state);

    assertEquals(state, state);
    assertEquals(newVectorFieldValue, newVectorFieldValue);
    assertNotSame(state, newVectorFieldValue);
    assertNotEquals(state, newVectorFieldValue);

    state = new CartesianVectorFieldValue(position, velocity);
    newVectorFieldValue = new CartesianVectorFieldValue(position, velocity);
    assertNotSame(state, newVectorFieldValue);
    assertEquals(state, newVectorFieldValue);

    state = new CartesianVectorFieldValue(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    newVectorFieldValue = new CartesianVectorFieldValue(new VectorIJK(x1 + 0.1, y1, z1),
        new VectorIJK(dx1, dy1, dz1));
    assertNotSame(state, newVectorFieldValue);
    assertNotEquals(state, newVectorFieldValue);

    state = new CartesianVectorFieldValue(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    newVectorFieldValue = new CartesianVectorFieldValue(new VectorIJK(x1, y1 + 0.1, z1),
        new VectorIJK(dx1, dy1, dz1));
    assertNotSame(state, newVectorFieldValue);
    assertNotEquals(state, newVectorFieldValue);

    state = new CartesianVectorFieldValue(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    newVectorFieldValue = new CartesianVectorFieldValue(new VectorIJK(x1, y1, z1 + 0.1),
        new VectorIJK(dx1, dy1, dz1));
    assertNotSame(state, newVectorFieldValue);
    assertNotEquals(state, newVectorFieldValue);

    state = new CartesianVectorFieldValue(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    newVectorFieldValue = new CartesianVectorFieldValue(new VectorIJK(x1, y1, z1),
        new VectorIJK(dx1 + 0.1, dy1, dz1));
    assertNotSame(state, newVectorFieldValue);
    assertNotEquals(state, newVectorFieldValue);

    state = new CartesianVectorFieldValue(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    newVectorFieldValue = new CartesianVectorFieldValue(new VectorIJK(x1, y1, z1),
        new VectorIJK(dx1, dy1 + 0.1, dz1));
    assertNotSame(state, newVectorFieldValue);
    assertNotEquals(state, newVectorFieldValue);

    state = new CartesianVectorFieldValue(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    newVectorFieldValue = new CartesianVectorFieldValue(new VectorIJK(x1, y1, z1),
        new VectorIJK(dx1, dy1, dz1 + 0.1));
    assertNotSame(state, newVectorFieldValue);
    assertNotEquals(state, newVectorFieldValue);

    state = new CartesianVectorFieldValue(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    newVectorFieldValue = null;
    assertNotSame(state, newVectorFieldValue);
    assertNotEquals(state, newVectorFieldValue);

    state = new CartesianVectorFieldValue(new VectorIJK(x1, y1, z1), new VectorIJK(dx1, dy1, dz1));
    assertNotEquals(state, new Object());

  }

  @Test
  public void testToString() {
    assertEquals("[position=[" + state.getPosition().getI() + "," + state.getPosition().getJ() + ","
        + state.getPosition().getK() + "], value=[" + state.getValue().getI() + ","
        + state.getValue().getJ() + "," + state.getValue().getK() + "]]", state.toString());
    assertEquals("[position=[" + newVectorFieldValue.getPosition().getI() + ","
        + newVectorFieldValue.getPosition().getJ() + "," + newVectorFieldValue.getPosition().getK()
        + "], value=[" + newVectorFieldValue.getValue().getI() + ","
        + newVectorFieldValue.getValue().getJ() + "," + newVectorFieldValue.getValue().getK()
        + "]]", newVectorFieldValue.toString());
  }

}
