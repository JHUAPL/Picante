package picante.math.coords;

import static java.lang.Math.atan2;
import static java.lang.Math.cos;
import static java.lang.Math.sin;
import static java.lang.Math.sqrt;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.VectorIJK;

public class VectorFieldValueConversionsTest {

  private final double x = -23.1;
  private final double y = 14.242;
  private final double z = 9.434;

  private final double fx = 42.2143;
  private final double fy = -34.4351;
  private final double fz = -3.6321;

  private double r;
  private double cr;
  private double phi;
  private double theta;
  private double h;

  private double fr;
  private double fcr;
  private double fphi;
  private double ftheta;
  private double fh;

  @Before
  public void setUp() throws Exception {

    r = sqrt(x * x + y * y + z * z);
    cr = sqrt(x * x + y * y);
    phi = atan2(y, x);
    theta = atan2(sqrt(x * x + y * y), z);
    h = z;

    fr = sin(theta) * cos(phi) * fx + sin(theta) * sin(phi) * fy + cos(theta) * fz;
    fcr = cos(phi) * fx + sin(phi) * fy;
    fphi = -sin(phi) * fx + cos(phi) * fy;
    ftheta = cos(theta) * cos(phi) * fx + cos(theta) * sin(phi) * fy - sin(theta) * fz;
    fh = fz;
  }

  @After
  public void tearDown() throws Exception {}

  @Test
  public void testConvertCylindricalVectorFieldValue() {

    double TOL = 5.0E-14;

    CylindricalVector position = new CylindricalVector(cr, phi, h);
    CylindricalVector value = new CylindricalVector(fcr, fphi, fh);

    CylindricalVectorFieldValue cylindrical = new CylindricalVectorFieldValue(position, value);

    CartesianVectorFieldValue cartesian = VectorFieldValueConversions.convert(cylindrical);

    assertEquals(x, cartesian.getPosition().getI(), TOL);
    assertEquals(y, cartesian.getPosition().getJ(), TOL);
    assertEquals(z, cartesian.getPosition().getK(), TOL);

    assertEquals(fx, cartesian.getValue().getI(), TOL);
    assertEquals(fy, cartesian.getValue().getJ(), TOL);
    assertEquals(fz, cartesian.getValue().getK(), TOL);
  }

  @Test
  public void testConvertToCylindrical() {

    double TOL = 5.0E-14;

    VectorIJK position = new VectorIJK(x, y, z);
    VectorIJK value = new VectorIJK(fx, fy, fz);

    CartesianVectorFieldValue cartesian = new CartesianVectorFieldValue(position, value);

    CylindricalVectorFieldValue cylindrical =
        VectorFieldValueConversions.convertToCylindrical(cartesian);

    assertEquals(cr, cylindrical.getPosition().getCylindricalRadius(), TOL);
    assertEquals(phi, cylindrical.getPosition().getLongitude(), TOL);
    assertEquals(z, cylindrical.getPosition().getHeight(), TOL);

    assertEquals(fcr, cylindrical.getValue().getCylindricalRadius(), TOL);
    assertEquals(fphi, cylindrical.getValue().getLongitude(), TOL);
    assertEquals(fz, cylindrical.getValue().getHeight(), TOL);

  }

  @Test
  public void testConvertSphericalVectorFieldValue() {

    double TOL = 5.0E-14;

    SphericalVector position = new SphericalVector(r, theta, phi);
    SphericalVector value = new SphericalVector(fr, ftheta, fphi);

    SphericalVectorFieldValue spherical = new SphericalVectorFieldValue(position, value);

    CartesianVectorFieldValue cartesian = VectorFieldValueConversions.convert(spherical);

    assertEquals(x, cartesian.getPosition().getI(), TOL);
    assertEquals(y, cartesian.getPosition().getJ(), TOL);
    assertEquals(z, cartesian.getPosition().getK(), TOL);

    assertEquals(fx, cartesian.getValue().getI(), TOL);
    assertEquals(fy, cartesian.getValue().getJ(), TOL);
    assertEquals(fz, cartesian.getValue().getK(), TOL);
  }

  @Test
  public void testConvertToSpherical() {

    double TOL = 5.0E-14;

    VectorIJK position = new VectorIJK(x, y, z);
    VectorIJK value = new VectorIJK(fx, fy, fz);

    CartesianVectorFieldValue cartesian = new CartesianVectorFieldValue(position, value);

    SphericalVectorFieldValue spherical = VectorFieldValueConversions.convertToSpherical(cartesian);

    assertEquals(r, spherical.getPosition().getRadius(), TOL);
    assertEquals(phi, spherical.getPosition().getLongitude(), TOL);
    assertEquals(theta, spherical.getPosition().getColatitude(), TOL);

    assertEquals(fr, spherical.getValue().getRadius(), TOL);
    assertEquals(fphi, spherical.getValue().getLongitude(), TOL);
    assertEquals(ftheta, spherical.getValue().getColatitude(), TOL);
  }

  @Test
  public void testConstructorIsPrivate() throws NoSuchMethodException, IllegalAccessException,
      InvocationTargetException, InstantiationException {
    Constructor<VectorFieldValueConversions> constructor =
        VectorFieldValueConversions.class.getDeclaredConstructor();
    assertTrue(Modifier.isPrivate(constructor.getModifiers()));
    constructor.setAccessible(true);
    constructor.newInstance();
  }

}
