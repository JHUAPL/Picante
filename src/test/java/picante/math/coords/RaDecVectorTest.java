package picante.math.coords;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import org.junit.Before;
import org.junit.Test;

public class RaDecVectorTest {

  private double radius = 3.0;
  private double ra = 2.0;
  private double dec = 1.0;

  private RaDecVector coord1;
  private RaDecVector coord2;

  @Before
  public void setUp() throws Exception {
    coord1 = new RaDecVector(12.0, .21342, 1.102);
    coord2 = new RaDecVector(3.0, 1.2123, -.102);
  }

  @Test
  public void testRaDecCoord() {
    coord1 = RaDecVector.ZERO;
    assertEquals(RaDecVector.ZERO, coord1);
  }

  @Test
  public void testRaDecCoordDoubleDoubleDouble() {
    coord1 =
        new RaDecVector(coord2.getRadius(), coord2.getRightAscension(), coord2.getDeclination());
    assertEquals(coord2, coord1);
  }

  // @Test
  // public void testRaDecCoordDoubleArray() {
  // coord1 =
  // new RaDecVector(new double[] {coord2.getRadius(), coord2.getColatitude(),
  // coord2.getLongitude()});
  // assertEquals(coord2, coord1);
  // }

  @Test
  public void testUnwritableRaDecCoordDoubleDoubleDouble() {
    coord1 = new RaDecVector(radius, ra, dec);
    assertEquals(radius, coord1.getRadius(), 0.0);
    assertEquals(ra, coord1.getRightAscension(), 0.0);
    assertEquals(dec, coord1.getDeclination(), 0.0);
  }

  // @Test
  // public void testUnwritableRaDecCoordDoubleArray() {
  // coord1 = new RaDecVector(new double[] {radius, colat, lon});
  // assertEquals(radius, coord1.getRadius(), 0.0);
  // assertEquals(colat, coord1.getColatitude(), 0.0);
  // assertEquals(lon, coord1.getLongitude(), 0.0);
  // }

  @Test
  public void testGetRadius() {
    assertEquals(12.0, coord1.getRadius(), 0.0);
    assertEquals(3.0, coord2.getRadius(), 0.0);
  }

  @Test
  public void testGetRightAscension() {
    assertEquals(.21342, coord1.getRightAscension(), 0.0);
    assertEquals(1.2123, coord2.getRightAscension(), 0.0);
  }

  @Test
  public void testGetDeclination() {
    assertEquals(1.102, coord1.getDeclination(), 0.0);
    assertEquals(-.102, coord2.getDeclination(), 0.0);
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
    assertEquals(
        "RaDecVector [radius: " + coord1.getRadius() + ", rightAscension: "
            + coord1.getRightAscension() + ", declination: " + coord1.getDeclination() + "]",
        coord1.toString());
    assertEquals(
        "RaDecVector [radius: " + coord2.getRadius() + ", rightAscension: "
            + coord2.getRightAscension() + ", declination: " + coord2.getDeclination() + "]",
        coord2.toString());
  }

}
