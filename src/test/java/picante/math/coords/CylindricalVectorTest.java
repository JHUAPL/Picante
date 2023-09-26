package picante.math.coords;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotSame;
import org.junit.Before;
import org.junit.Test;

public class CylindricalVectorTest {

  private double radius = 3.0;
  private double height = 4.0;
  private double lon = 1.0;

  private CylindricalVector coord1;
  private CylindricalVector coord2;

  @Before
  public void setUp() throws Exception {
    coord1 = new CylindricalVector(12.0, .21342, 1.102);
    coord2 = new CylindricalVector(3.0, 1.2123, -.102);
  }

  @Test
  public void testCylindricalCoord() {
    coord1 = CylindricalVector.ZERO;
    assertEquals(CylindricalVector.ZERO, coord1);
  }

  @Test
  public void testCylindricalCoordDoubleDoubleDouble() {
    coord1 = new CylindricalVector(coord2.getCylindricalRadius(), coord2.getLongitude(),
        coord2.getHeight());
    assertEquals(coord2, coord1);
  }

  // @Test
  // public void testCylindricalCoordDoubleArray() {
  // coord1 =
  // new CylindricalVector(new double[] {coord2.getCylindricalRadius(), coord2.getLongitude(),
  // coord2.getHeight()});
  // assertEquals(coord2, coord1);
  // }


  @Test
  public void testUnwritableCylindricalCoordDoubleDoubleDouble() {
    coord1 = new CylindricalVector(radius, lon, height);
    assertEquals(radius, coord1.getCylindricalRadius(), 0.0);
    assertEquals(lon, coord1.getLongitude(), 0.0);
    assertEquals(height, coord1.getHeight(), 0.0);
  }

  // @Test
  // public void testUnwritableCylindricalCoordDoubleArray() {
  // coord1 = new CylindricalVector(new double[] {radius, lon, height});
  // assertEquals(radius, coord1.getCylindricalRadius(), 0.0);
  // assertEquals(lon, coord1.getLongitude(), 0.0);
  // assertEquals(height, coord1.getHeight(), 0.0);
  // }

  @Test
  public void testGetCylindricalRadius() {
    assertEquals(12.0, coord1.getCylindricalRadius(), 0.0);
    assertEquals(3.0, coord2.getCylindricalRadius(), 0.0);
  }

  @Test
  public void testGetLongitude() {
    assertEquals(.21342, coord1.getLongitude(), 0.0);
    assertEquals(1.2123, coord2.getLongitude(), 0.0);
  }

  @Test
  public void testGetHeight() {
    assertEquals(1.102, coord1.getHeight(), 0.0);
    assertEquals(-.102, coord2.getHeight(), 0.0);
  }

  @Test
  public void testHashCode() {
    assertEquals(coord1.hashCode(), coord1.hashCode());
    assertEquals(coord2.hashCode(), coord2.hashCode());
    assertNotSame(coord1.hashCode(), coord2.hashCode());
  }

  @Test
  public void testEqualsObject() {

    assertEquals(coord1.getVectorIJK(), coord1.getVectorIJK());

    assertEquals(coord1, coord1);
    assertEquals(coord2, coord2);
    assertNotSame(coord1, coord2);
    assertNotEquals(coord1, coord2);

    coord1 = new CylindricalVector(radius, height, lon);
    coord2 = new CylindricalVector(radius, height, lon);
    assertNotSame(coord1, coord2);
    assertEquals(coord1, coord2);

    coord1 = new CylindricalVector(radius, height, lon);
    coord2 = new CylindricalVector(radius + 0.1, height, lon);
    assertNotSame(coord1, coord2);
    assertNotEquals(coord1, coord2);

    coord1 = new CylindricalVector(radius, height, lon);
    coord2 = new CylindricalVector(radius, height + 0.1, lon);
    assertNotSame(coord1, coord2);
    assertNotEquals(coord1, coord2);

    coord1 = new CylindricalVector(radius, height, lon);
    coord2 = new CylindricalVector(radius, height, lon + 0.1);
    assertNotSame(coord1, coord2);
    assertNotEquals(coord1, coord2);

    coord1 = new CylindricalVector(radius, height, lon);
    coord2 = null;
    assertNotSame(coord1, coord2);
    assertNotEquals(coord1, coord2);

    coord1 = new CylindricalVector(radius, height, lon);
    assertNotEquals(coord1, new Object());

  }

  @Test
  public void testToString() {
    assertEquals("CylindricalVector [cylindricalRadius: " + coord1.getCylindricalRadius()
        + ", longitude: " + coord1.getLongitude() + ", height: " + coord1.getHeight() + "]",
        coord1.toString());
    assertEquals("CylindricalVector [cylindricalRadius: " + coord2.getCylindricalRadius()
        + ", longitude: " + coord2.getLongitude() + ", height: " + coord2.getHeight() + "]",
        coord2.toString());
  }

}
