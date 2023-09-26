package picante.math.coords;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import org.junit.Test;

public class CoordUtilitiesTest {
  private final double tol = 1.e-12;

  @Test
  public void testColatToLat() {
    testOneColatToLat(Math.toRadians(90), 0.0);
    testOneColatToLat(Math.toRadians(45), Math.toRadians(45.0));
    testOneColatToLat(Math.toRadians(5), Math.toRadians(85.0));
    testOneColatToLat(Math.toRadians(105), Math.toRadians(-15.0));

    // Do one test that uses existing converters to find the latitude.
    double colat = Math.toRadians(91);
    double lat = CoordConverters
        .convertToLatitudinal(CoordConverters.convert(new SphericalVector(1.0, colat, 0.0)))
        .getLatitude();
    testOneColatToLat(colat, lat);
  }

  @Test
  public void testLatToColat() {
    testOneLatToColat(Math.toRadians(90), 0.0);
    testOneLatToColat(Math.toRadians(45), Math.toRadians(45.0));
    testOneLatToColat(Math.toRadians(5), Math.toRadians(85.0));
    testOneLatToColat(Math.toRadians(-15), Math.toRadians(105.0));

    // Do one test that uses existing converters to find the colatitude.
    double lat = Math.toRadians(1);
    double colat = CoordConverters
        .convertToSpherical(CoordConverters.convert(new LatitudinalVector(1.0, lat, 0.0)))
        .getColatitude();
    testOneLatToColat(lat, colat);
  }

  private void testOneColatToLat(double colat, double actualLat) {
    double calculatedLat = CoordUtilities.toLatitude(colat);
    assertEquals("converting colat -> lat", calculatedLat, actualLat, tol);
  }

  private void testOneLatToColat(double lat, double actualColat) {
    double calculatedColat = CoordUtilities.toColatitude(lat);
    assertEquals("converting colat -> lat", calculatedColat, actualColat, tol);
  }

  @Test
  public void testConstructorIsPrivate()
      throws NoSuchMethodException, SecurityException, InstantiationException,
      IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    Constructor<CoordUtilities> constructor = CoordUtilities.class.getDeclaredConstructor();
    assertTrue(Modifier.isPrivate(constructor.getModifiers()));
    constructor.setAccessible(true);
    constructor.newInstance();
  }

}
