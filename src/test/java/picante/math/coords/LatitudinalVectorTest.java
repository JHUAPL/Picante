package picante.math.coords;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import org.junit.Before;
import org.junit.Test;

public class LatitudinalVectorTest {

  private double radius = 3.0;
  private double colat = 2.0;
  private double lon = 1.0;

  private LatitudinalVector coord1;
  private LatitudinalVector coord2;

  @Before
  public void setUp() throws Exception {
    coord1 = new LatitudinalVector(12.0, .21342, 1.102);
    coord2 = new LatitudinalVector(3.0, 1.2123, -.102);
  }

  @Test
  public void testLatitudinalCoord() {
    coord1 = LatitudinalVector.ZERO;
    assertEquals(LatitudinalVector.ZERO, coord1);
  }

  @Test
  public void testLatitudinalCoordDoubleDoubleDouble() {
    coord1 = new LatitudinalVector(coord2.getRadius(), coord2.getLatitude(), coord2.getLongitude());
    assertEquals(coord2, coord1);
  }

  // @Test
  // public void testLatitudinalCoordDoubleArray() {
  // coord1 =
  // new LatitudinalVector(new double[] {coord2.getRadius(), coord2.getLatitude(),
  // coord2.getLongitude()});
  // assertEquals(coord2, coord1);
  // }

  @Test
  public void testUnwritableLatitudinalCoordDoubleDoubleDouble() {
    coord1 = new LatitudinalVector(radius, colat, lon);
    assertEquals(radius, coord1.getRadius(), 0.0);
    assertEquals(colat, coord1.getLatitude(), 0.0);
    assertEquals(lon, coord1.getLongitude(), 0.0);
  }

  // @Test
  // public void testUnwritableLatitudinalCoordDoubleArray() {
  // coord1 = new LatitudinalVector(new double[] {radius, colat, lon});
  // assertEquals(radius, coord1.getRadius(), 0.0);
  // assertEquals(colat, coord1.getLatitude(), 0.0);
  // assertEquals(lon, coord1.getLongitude(), 0.0);
  // }

  @Test
  public void testGetRadius() {
    assertEquals(12.0, coord1.getRadius(), 0.0);
    assertEquals(3.0, coord2.getRadius(), 0.0);
  }

  @Test
  public void testGetLatitude() {
    assertEquals(.21342, coord1.getLatitude(), 0.0);
    assertEquals(1.2123, coord2.getLatitude(), 0.0);
  }

  @Test
  public void testGetLongitude() {
    assertEquals(1.102, coord1.getLongitude(), 0.0);
    assertEquals(-.102, coord2.getLongitude(), 0.0);
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
  }

  @Test
  public void testToString() {
    assertEquals("LatitudinalVector [radius: " + coord1.getRadius() + ", latitude: "
        + coord1.getLatitude() + ", longitude: " + coord1.getLongitude() + "]", coord1.toString());
    assertEquals("LatitudinalVector [radius: " + coord2.getRadius() + ", latitude: "
        + coord2.getLatitude() + ", longitude: " + coord2.getLongitude() + "]", coord2.toString());
  }

}
