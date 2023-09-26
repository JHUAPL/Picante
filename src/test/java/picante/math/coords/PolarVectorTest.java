package picante.math.coords;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotSame;
import org.junit.Before;
import org.junit.Test;

public class PolarVectorTest {

  private double radius = 3.0;
  private double angle = 2.0;


  private PolarVector coord1;
  private PolarVector coord2;

  @Before
  public void setUp() throws Exception {
    coord1 = new PolarVector(12.0, .21342);
    coord2 = new PolarVector(3.0, 1.2123);
  }

  @Test
  public void testPolarCoord() {
    coord1 = PolarVector.ZERO;
    assertEquals(PolarVector.ZERO, coord1);
  }

  @Test
  public void testPolarCoordDoubleDoubleDouble() {
    coord1 = new PolarVector(coord2.getRadius(), coord2.getAngle());
    assertEquals(coord2, coord1);
  }

  @Test
  public void testPolarCoordDoubleDoubleDouble2() {
    coord1 = new PolarVector(radius, angle);
    assertEquals(radius, coord1.getRadius(), 0.0);
    assertEquals(angle, coord1.getAngle(), 0.0);
  }

  // @Test
  // public void testUnwritablePolarCoordDoubleArray() {
  // coord1 = new PolarVector(new double[] {radius, angle});
  // assertEquals(radius, coord1.getRadius(), 0.0);
  // assertEquals(angle, coord1.getAngle(), 0.0);
  // }

  @Test
  public void testGetRadius() {
    assertEquals(12.0, coord1.getRadius(), 0.0);
    assertEquals(3.0, coord2.getRadius(), 0.0);
  }

  @Test
  public void testGetAngle() {
    assertEquals(.21342, coord1.getAngle(), 0.0);
    assertEquals(1.2123, coord2.getAngle(), 0.0);
  }

  @Test
  public void testHashCode() {
    assertEquals(coord1.hashCode(), coord1.hashCode());
    assertEquals(coord2.hashCode(), coord2.hashCode());
    assertNotSame(coord1.hashCode(), coord2.hashCode());
  }

  @Test
  public void testEqualsObject() throws NoSuchFieldException, SecurityException {

    assertEquals(coord1.getVectorIJ(), coord1.getVectorIJ());

    assertEquals(coord1, coord1);
    assertEquals(coord2, coord2);
    assertNotSame(coord1, coord2);
    assertNotEquals(coord1, coord2);

    coord1 = new PolarVector(radius, angle);
    coord2 = new PolarVector(radius, angle);
    assertNotSame(coord1, coord2);
    assertEquals(coord1, coord2);

    coord1 = new PolarVector(radius, angle);
    coord2 = new PolarVector(radius + .1, angle);
    assertNotSame(coord1, coord2);
    assertNotEquals(coord1, coord2);

    coord1 = new PolarVector(radius, angle);
    coord2 = new PolarVector(radius, angle + .1);
    assertNotSame(coord1, coord2);
    assertNotEquals(coord1, coord2);

    coord1 = new PolarVector(radius, angle);
    coord2 = null;
    assertNotSame(coord1, coord2);
    assertNotEquals(coord1, coord2);

    coord1 = new PolarVector(radius, angle);
    assertNotEquals(coord1, new Object());

  }

  @Test
  public void testToString() {
    assertEquals(
        "PolarVector [radius: " + coord1.getRadius() + ", angle: " + coord1.getAngle() + "]",
        coord1.toString());
    assertEquals(
        "PolarVector [radius: " + coord2.getRadius() + ", angle: " + coord2.getAngle() + "]",
        coord2.toString());
  }

}
