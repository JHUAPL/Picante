package picante.math.coords;

import static java.lang.Math.PI;
import static java.lang.Math.atan2;
import static java.lang.Math.sqrt;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static picante.math.coords.AssertTools.assertComponentEquals;
import static picante.units.FundamentalPhysicalConstants.HALFPI;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import picante.math.vectorspace.UnwritableVectorIJ;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJ;
import picante.math.vectorspace.VectorIJK;
import picante.units.FundamentalPhysicalConstants;

public class CoordConvertersTest {

  private static final double TOL = 1.e-13;

  // VectorIJKs
  private VectorIJK zero = new VectorIJK(0.0, 0.0, 0.0);

  private VectorIJK plusX = new VectorIJK(+1.0, 0.0, 0.0);
  private VectorIJK negX = new VectorIJK(-1.0, 0.0, 0.0);

  private VectorIJK plusY = new VectorIJK(0.0, +1.0, 0.0);
  private VectorIJK negY = new VectorIJK(0.0, -1.0, 0.0);

  private VectorIJK plusZ = new VectorIJK(0.0, 0.0, +1.0);
  private VectorIJK negZ = new VectorIJK(0.0, 0.0, -1.0);

  // SphericalCoords
  private SphericalVector zeroS = new SphericalVector(0.0, 0.0, 0.0);

  private SphericalVector plusXS = new SphericalVector(1.0, HALFPI, 0.0);
  private SphericalVector negXS = new SphericalVector(1.0, HALFPI, Math.PI);

  private SphericalVector plusYS = new SphericalVector(1.0, HALFPI, HALFPI);
  private SphericalVector negYS = new SphericalVector(1.0, HALFPI, -HALFPI);

  private SphericalVector plusZS = new SphericalVector(1.0, 0.0, 0.0);
  private SphericalVector negZS = new SphericalVector(1.0, Math.PI, 0.0);

  @Before
  public void setUp() throws Exception {}

  @Test
  public void testConvertVectorIJKSphericalCoord() {

    assertComponentEquals(zeroS, CoordConverters.convertToSpherical(zero), TOL);

    assertComponentEquals(plusXS, CoordConverters.convertToSpherical(plusX), TOL);

    assertComponentEquals(negXS, CoordConverters.convertToSpherical(negX), TOL);

    assertComponentEquals(plusYS, CoordConverters.convertToSpherical(plusY), TOL);

    assertComponentEquals(negYS, CoordConverters.convertToSpherical(negY), TOL);

    assertComponentEquals(plusZS, CoordConverters.convertToSpherical(plusZ), TOL);

    assertComponentEquals(negZS, CoordConverters.convertToSpherical(negZ), TOL);
  }

  @Test
  public void testConvertSphericalCoordVectorIJK() {

    assertComponentEquals(zero, CoordConverters.convert(zeroS), TOL);

    assertComponentEquals(plusX, CoordConverters.convert(plusXS), TOL);

    assertComponentEquals(negX, CoordConverters.convert(negXS), TOL);

    assertComponentEquals(plusY, CoordConverters.convert(plusYS), TOL);

    assertComponentEquals(negY, CoordConverters.convert(negYS), TOL);

    assertComponentEquals(plusZ, CoordConverters.convert(plusZS), TOL);

    assertComponentEquals(negZ, CoordConverters.convert(negZS), TOL);
  }

  @Test
  public void testConvertUnwritableCartesianStateSphericalState() {

    CartesianState cartState =
        new CartesianState(new VectorIJK(-235.3474376457, -12.325432453425, 23.324512543254324),
            new VectorIJK(10.21342134213423, .21223124213421, -1.1234234687444356));

    SphericalState spiceSphState = new SphericalState(
        new SphericalVector(236.82137905379452, 1.4721465040166326, -3.0892692351599216),
        new SphericalVector(-10.271546001521225, 4.74307324385180376E-004,
            1.36723624375647201E-003));

    assertComponentEquals(spiceSphState, CoordConverters.convertToSpherical(cartState), TOL);
  }

  @Test
  public void testConvertUnwritableSphericalStateCartesianState() {
    SphericalState sphState = new SphericalState(
        new SphericalVector(236.82137905379452, 1.4721465040166326, -3.0892692351599216),
        new SphericalVector(-10.271546001521225, 4.74307324385180376E-004,
            1.36723624375647201E-003));

    CartesianState expectedCart =
        new CartesianState(new VectorIJK(-235.3474376457, -12.325432453425, 23.324512543254324),
            new VectorIJK(10.21342134213423, .21223124213421, -1.1234234687444356));

    CartesianState spiceCart = new CartesianState(
        new VectorIJK(-235.34743764570001, -12.325432453425000, 23.324512543254325),
        new VectorIJK(10.213421342134232, 0.21223124213421013, -1.1234234687444358));

    assertComponentEquals(expectedCart, CoordConverters.convert(sphState), TOL);
    assertComponentEquals(spiceCart, expectedCart, TOL);
  }

  @Test
  @Ignore
  public void testConvertUnwritableVectorIJKRaDecCoord() {
    fail("Not yet implemented");
  }

  @Test
  @Ignore
  public void testConvertUnwritableRaDecCoordVectorIJK() {
    fail("Not yet implemented");
  }

  /**
   * Testing the positive Z axis
   */
  @Test
  public void testZAxis1() {
    UnwritableVectorIJK cart = VectorIJK.K;
    CylindricalVector cyl = new CylindricalVector(0, 0, 1);

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);
    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }

  /**
   * Testing the positive X axis
   */
  @Test
  public void testXAxis() {
    UnwritableVectorIJK cart = VectorIJK.I;
    CylindricalVector cyl = new CylindricalVector(1, 0, 0);

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);
    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }

  /**
   * Testing the negative X axis
   */
  @Test
  public void testMinusXAxis() {
    // Note, the commented out code WILL fail
    // UnwritableVectorIJK cart = VectorIJK.I.createNegated();
    UnwritableVectorIJK cart = VectorIJK.MINUS_I;
    CylindricalVector cyl = new CylindricalVector(1, PI, 0);

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);
    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }

  /**
   * Testing the positive Y axis
   */
  @Test
  public void testYAxis() {
    UnwritableVectorIJK cart = VectorIJK.J;
    CylindricalVector cyl = new CylindricalVector(1, PI / 2.0, 0);

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);
    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }

  /**
   * Testing the negative Y axis
   */
  @Test
  public void testMinusYAxis() {
    UnwritableVectorIJK cart = VectorIJK.MINUS_J;
    CylindricalVector cyl = new CylindricalVector(1, 3.0 * PI / 2.0, 0);

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);
    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }

  /**
   * Testing the unitized vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeUnitVector() {
    UnwritableVectorIJK cart = new VectorIJK(1, 1, 1).createUnitized();
    CylindricalVector cyl = new CylindricalVector(sqrt(2.0 / 3.0), PI / 4.0, 1.0 / sqrt(3.0));

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);
    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }

  /**
   * Testing the vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeVector() {
    UnwritableVectorIJK cart = new VectorIJK(1, 1, 1);
    CylindricalVector cyl = new CylindricalVector(sqrt(2.0), PI / 4.0, 1.0);

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);
    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }

  /**
   * Testing a random negative vector
   */
  @Test
  public void testNegativeVector() {
    UnwritableVectorIJK cart = new VectorIJK(-1, -2, -3);
    CylindricalVector cyl =
        new CylindricalVector(sqrt(1 + 4), atan2(-2, -1) + FundamentalPhysicalConstants.TWOPI, -3);

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);
    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }

  /**
   * Testing a random vector as compared directly to JPL's Spice library
   */
  @Test
  public void testWithPrecalcNumsA() {
    UnwritableVectorIJK cart = new UnwritableVectorIJK(7, -2, 5);
    // JPL's SPICE library gives these values for R, colat, and long:
    double r = 7.2801098892805189;
    double longitude = 6.0048856481744748;
    double height = 5.0;
    CylindricalVector cyl = new CylindricalVector(r, longitude, height);

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);
    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }

  /**
   * Testing the positive Z axis
   */
  @Test
  public void testZAxis1State() {
    CartesianState cart = new CartesianState(VectorIJK.K, VectorIJK.K);
    CylindricalState cyl =
        new CylindricalState(new CylindricalVector(0, 0, 1), new CylindricalVector(0, 0, 1));

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);
  }

  /**
   * Testing the positive Z axis
   */
  @Test(expected = PointOnAxisException.class)
  public void testZAxis1StateFail() {
    CartesianState cart = new CartesianState(VectorIJK.K, VectorIJK.K);
    CylindricalState cyl =
        new CylindricalState(new CylindricalVector(0, 0, 1), new CylindricalVector(0, 0, 1));

    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }

  /**
   * Testing the positive X axis
   */
  @Test
  public void testXAxisState() {
    CartesianState cart = new CartesianState(VectorIJK.I, VectorIJK.I);
    CylindricalState cyl =
        new CylindricalState(new CylindricalVector(1, 0, 0), new CylindricalVector(1, 0, 0));

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);
    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }

  /**
   * Testing the negative X axis
   */
  @Test
  public void testMinusXAxisState() {
    // Note, the commented out code WILL fail
    // UnwritableVectorIJK cart = VectorIJK.I.createNegated();
    CartesianState cart = new CartesianState(VectorIJK.MINUS_I, VectorIJK.MINUS_I);
    CylindricalState cyl =
        new CylindricalState(new CylindricalVector(1, Math.PI, 0), new CylindricalVector(1, 0, 0));

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);

    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }

  /**
   * Testing the positive Y axis
   */
  @Test
  public void testYAxisState() {
    CartesianState cart = new CartesianState(VectorIJK.J, VectorIJK.J);
    CylindricalState cyl =
        new CylindricalState(new CylindricalVector(1, PI / 2.0, 0), new CylindricalVector(1, 0, 0));

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);

    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }

  /**
   * Testing the unitized vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeUnitVectorState() {
    CartesianState cart = new CartesianState(new VectorIJK(1, 1, 1).createUnitized(),
        new VectorIJK(1, 1, 1).createUnitized());
    CylindricalState cyl = new CylindricalState(
        new CylindricalVector(0.81649658092772615, 0.78539816339744828, 0.57735026918962584),
        new CylindricalVector(0.81649658092772603, 0.0, 0.57735026918962584));

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);

    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }

  /**
   * Testing the vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeVectorState() {
    CartesianState cart = new CartesianState(new VectorIJK(1, 1, 1), new VectorIJK(1, 1, 1));
    CylindricalState cyl =
        new CylindricalState(new CylindricalVector(1.4142135623730951, 0.78539816339744828, 1.0),
            new CylindricalVector(1.4142135623730949, 0, 1.0));

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);

    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }

  /**
   * Testing a random negative vector
   */
  @Test
  public void testNegativeVectorState() {
    CartesianState cart = new CartesianState(new VectorIJK(-1, -2, -3), new VectorIJK(-1, -2, -3));
    CylindricalState cyl =
        new CylindricalState(new CylindricalVector(2.2360679774997898, 4.2487413713838835, -3.0),
            new CylindricalVector(2.2360679774997902, 0.0, -3.0));

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);
    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }

  /**
   * Testing a random vector as compared directly to JPL's Spice library
   */
  @Test
  public void testWithPrecalcNumsAState() {
    CartesianState cart = new CartesianState(new VectorIJK(7, -2, 5),
        new VectorIJK(1.21342134213423, 0.21223124213421, -1.1234234687444356));
    // JPL's SPICE library gives these values for R, colat, and long:
    double r = 7.2801098892805189;
    double longitude = 6.0048856481744748;
    double height = 5.0;
    CylindricalState cyl = new CylindricalState(new CylindricalVector(r, longitude, height),
        new CylindricalVector(1.1084292728263583, 7.38200260227911687E-2, -1.1234234687444355));

    assertComponentEquals(cart, CoordConverters.convert(cyl), TOL);

    assertComponentEquals(cyl, CoordConverters.convertToCylindrical(cart), TOL);
  }


  /**
   * Testing the unitized vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeUnitVectorLatitudinal() {
    UnwritableVectorIJK cart = new VectorIJK(1, 1, 1).createUnitized();
    double lati = CoordUtilities.toLatitude(Math.atan2(sqrt(2.0 / 3.0), sqrt(1.0 / 3.0)));
    LatitudinalVector sph = new LatitudinalVector(1, lati, PI / 4.0);

    assertComponentEquals(cart, CoordConverters.convert(sph), TOL);
    assertComponentEquals(sph, CoordConverters.convertToLatitudinal(cart), TOL);
  }

  /**
   * Testing the unitized vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeUnitVectorStateLatitudinal() {
    CartesianState cart = new CartesianState(new VectorIJK(1, 1, 1).createUnitized(),
        new VectorIJK(1, 1, 1).createUnitized());
    double lati = CoordUtilities.toLatitude(Math.atan2(sqrt(2.0 / 3.0), sqrt(1.0 / 3.0)));
    LatitudinalState lat = new LatitudinalState(new LatitudinalVector(1, lati, PI / 4.0),
        new LatitudinalVector(1, 0, 0));

    assertComponentEquals(cart, CoordConverters.convert(lat), TOL);
    assertComponentEquals(lat, CoordConverters.convertToLatitudinal(cart), TOL);
  }

  /**
   * Testing the unitized vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeUnitVectorRaDec() {
    UnwritableVectorIJK cart = new VectorIJK(1, 1, 1).createUnitized();
    double lati = CoordUtilities.toLatitude(Math.atan2(sqrt(2.0 / 3.0), sqrt(1.0 / 3.0)));
    RaDecVector sph = new RaDecVector(1, PI / 4.0, lati);

    assertComponentEquals(cart, CoordConverters.convert(sph), TOL);
    assertComponentEquals(sph, CoordConverters.convertToRaDec(cart), TOL);
  }

  @Test
  public void testConstructorIsPrivate() throws NoSuchMethodException, IllegalAccessException,
      InvocationTargetException, InstantiationException {
    Constructor<CoordConverters> constructor = CoordConverters.class.getDeclaredConstructor();
    assertTrue(Modifier.isPrivate(constructor.getModifiers()));
    constructor.setAccessible(true);
    constructor.newInstance();
  }

  @Test
  public void testToCoordinateUnwritableVectorIJ() {

    VectorIJ vector1 = new VectorIJ(1, 0);
    VectorIJ vector2 = new VectorIJ(88.1234, 88.1234);
    VectorIJ vector3 = new VectorIJ(0, 2);
    VectorIJ vector4 = new VectorIJ(-3, 0);
    VectorIJ vector5 = new VectorIJ(0, -4);
    VectorIJ vector6 = new VectorIJ(0, 0);

    PolarVector polar1 = new PolarVector(1, 0);
    PolarVector polar2 = new PolarVector(Math.sqrt(2) * 88.1234, Math.PI / 4);
    PolarVector polar3 = new PolarVector(2, Math.PI / 2);
    PolarVector polar4 = new PolarVector(3, Math.PI);
    PolarVector polar5 = new PolarVector(4, -Math.PI / 2);
    PolarVector polar6 = new PolarVector(0.0, 0);

    assertComponentEquals(polar1, CoordConverters.convertToPolar(vector1), 0.0);
    assertComponentEquals(polar2, CoordConverters.convertToPolar(vector2), 0.0);
    assertComponentEquals(polar3, CoordConverters.convertToPolar(vector3), 0.0);
    assertComponentEquals(polar4, CoordConverters.convertToPolar(vector4), 0.0);
    assertComponentEquals(polar5, CoordConverters.convertToPolar(vector5), 0.0);
    assertComponentEquals(polar6, CoordConverters.convertToPolar(vector6), 0.0);
  }

  @Test
  public void testToCartesianPolarVector() {

    PolarVector polar1 = new PolarVector(1, 0);
    PolarVector polar2 = new PolarVector(Math.sqrt(2) * 88.1234, Math.PI / 4);
    PolarVector polar3 = new PolarVector(2, Math.PI / 2);
    PolarVector polar4 = new PolarVector(3, Math.PI);
    PolarVector polar5 = new PolarVector(4, -Math.PI / 2);
    PolarVector polar6 = new PolarVector(0.0, 0);


    VectorIJ vector1 = new VectorIJ(1, 0);
    VectorIJ vector2 = new VectorIJ(88.1234, 88.1234);
    VectorIJ vector3 = new VectorIJ(0, 2);
    VectorIJ vector4 = new VectorIJ(-3, 0);
    VectorIJ vector5 = new VectorIJ(0, -4);
    VectorIJ vector6 = new VectorIJ(0, 0);

    assertComponentEquals(vector1, CoordConverters.convert(polar1), 0.0);
    assertComponentEquals(vector2, CoordConverters.convert(polar2), TOL);
    assertComponentEquals(vector3, CoordConverters.convert(polar3), TOL);
    assertComponentEquals(vector4, CoordConverters.convert(polar4), TOL);
    assertComponentEquals(vector5, CoordConverters.convert(polar5), TOL);
    assertComponentEquals(vector6, CoordConverters.convert(polar6), TOL);
  }

  @Test
  public void testToCoordinateStateOfUnwritableVectorIJ() {

    CartesianStateIJ state1 =
        new CartesianStateIJ(new UnwritableVectorIJ(5, -12), new UnwritableVectorIJ(0, 0));
    CartesianStateIJ state2 =
        new CartesianStateIJ(new UnwritableVectorIJ(5, -12), new UnwritableVectorIJ(-.5, 2.0));

    PolarState polar1 =
        new PolarState(new PolarVector(13, -1.176005207095135102), new PolarVector(0, 0));
    PolarState polar2 = new PolarState(new PolarVector(13, -1.176005207095135102),
        new PolarVector(getdrdt(5, -12, -.5, 2.0), getdthetadt(5, -12, -.5, 2.0)));

    assertComponentEquals(polar1, CoordConverters.convertToPolar(state1), TOL);
    assertComponentEquals(polar2, CoordConverters.convertToPolar(state2), TOL);

  }

  @Test(expected = PointOnAxisException.class)
  public void testToCoordinateStateOfUnwritableVectorIJException() {

    CartesianStateIJ state1 =
        new CartesianStateIJ(new UnwritableVectorIJ(0, 0), new UnwritableVectorIJ(3, -.5));

    CoordConverters.convertToPolar(state1);
  }

  @Test
  public void testToCartesianStateOfC() {

    PolarState polar1 =
        new PolarState(new PolarVector(13, -1.176005207095135102), new PolarVector(0, 0));
    PolarState polar2 = new PolarState(new PolarVector(13, -1.176005207095135102),
        new PolarVector(getdrdt(5, -12, -.5, 2.0), getdthetadt(5, -12, -.5, 2.0)));



    CartesianStateIJ state1 =
        new CartesianStateIJ(new UnwritableVectorIJ(5, -12), new UnwritableVectorIJ(0, 0));
    CartesianStateIJ state2 =
        new CartesianStateIJ(new UnwritableVectorIJ(5, -12), new UnwritableVectorIJ(-.5, 2.0));

    assertComponentEquals(state1, CoordConverters.convert(polar1), TOL);
    assertComponentEquals(state2, CoordConverters.convert(polar2), TOL);
  }

  private final static double getdrdt(double x, double y, double dxdt, double dydt) {
    return (x * dxdt + y * dydt) / sqrt(x * x + y * y);
  }

  private final static double getdthetadt(double x, double y, double dxdt, double dydt) {
    return (x * dydt - y * dxdt) / (x * x + y * y);
  }


}
