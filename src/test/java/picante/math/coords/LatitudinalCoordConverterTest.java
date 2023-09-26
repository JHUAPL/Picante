package picante.math.coords;

import static java.lang.Math.PI;
import static java.lang.Math.sqrt;
import static picante.math.coords.AssertTools.assertComponentEquals;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

public class LatitudinalCoordConverterTest {

  private static final double TOL = 1.e-14;
  private LatitudinalCoordConverter converter;

  @Before
  public void setUp() throws Exception {
    converter = new LatitudinalCoordConverter();
  }

  /**
   * Testing the zero
   */
  @Test
  public void testZero() {
    UnwritableVectorIJK cart = VectorIJK.ZERO;
    LatitudinalVector lat = new LatitudinalVector(0, 0, 0);

    assertComponentEquals(cart, converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, converter.toCoordinate(cart), TOL);
  }


  /**
   * Testing the positive Z axis
   */
  @Test
  public void testZAxis1() {
    UnwritableVectorIJK cart = VectorIJK.K;
    LatitudinalVector lat = new LatitudinalVector(1, PI / 2, 0);

    assertComponentEquals(cart, converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive X axis
   */
  @Test
  public void testXAxis() {
    UnwritableVectorIJK cart = VectorIJK.I;
    LatitudinalVector lat = new LatitudinalVector(1, 0, 0);

    assertComponentEquals(cart, converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the negative X axis
   */
  @Test
  public void testMinusXAxis() {
    // Note, the commented out code WILL fail
    // UnwritableVectorIJK cart = VectorIJK.I.createNegated();
    UnwritableVectorIJK cart = VectorIJK.MINUS_I;
    LatitudinalVector lat = new LatitudinalVector(1, 0, PI);

    assertComponentEquals(cart, converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive Y axis
   */
  @Test
  public void testYAxis() {
    UnwritableVectorIJK cart = VectorIJK.J;
    LatitudinalVector lat = new LatitudinalVector(1, 0, PI / 2.0);

    assertComponentEquals(cart, converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the unitized vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeUnitVector() {
    UnwritableVectorIJK cart = new VectorIJK(1, 1, 1).createUnitized();
    double lati = CoordUtilities.toLatitude(Math.atan2(sqrt(2.0 / 3.0), sqrt(1.0 / 3.0)));
    LatitudinalVector sph = new LatitudinalVector(1, lati, PI / 4.0);

    assertComponentEquals(cart, converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeVector() {
    UnwritableVectorIJK cart = new VectorIJK(1, 1, 1);
    double lati = CoordUtilities.toLatitude(Math.atan2(sqrt(2.0 / 3.0), sqrt(1.0 / 3.0)));
    LatitudinalVector sph = new LatitudinalVector(sqrt(3), lati, PI / 4.0);

    assertComponentEquals(cart, converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing a random negative vector
   */
  @Test
  public void testNegativeVector() {
    UnwritableVectorIJK cart = new VectorIJK(-1, -2, -3);
    double lati = CoordUtilities.toLatitude(PI / 2.0 + Math.atan2(3, sqrt(5)));
    double longInRads = Math.atan2(-2, -1);
    LatitudinalVector lat = new LatitudinalVector(sqrt(1 + 4 + 9), lati, longInRads);

    assertComponentEquals(cart, converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing a random vector as compared directly to JPL's Spice library
   */
  @Test
  public void testWithPrecalcNumsA() {
    UnwritableVectorIJK cart = new UnwritableVectorIJK(7, -2, 5);
    // JPL's SPICE library gives these values for R, lati, and long:
    double r = 8.831760866327848;
    double latitude = CoordUtilities.toLatitude(0.968982551591638);
    double longitude = -0.278299659005111;
    LatitudinalVector lat = new LatitudinalVector(r, latitude, longitude);

    assertComponentEquals(cart, converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive Z axis
   */
  @Test
  public void testZAxis1State() {
    CartesianState cart = new CartesianState(VectorIJK.K, VectorIJK.K);
    LatitudinalState lat =
        new LatitudinalState(new LatitudinalVector(1, PI / 2., 0), new LatitudinalVector(1, 0, 0));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(lat), TOL);
  }

  /**
   * Testing the positive Z axis
   */
  @Test(expected = PointOnAxisException.class)
  public void testZAxis1StateFail() {
    CartesianState cart = new CartesianState(VectorIJK.K, VectorIJK.K);
    LatitudinalState sph =
        new LatitudinalState(new LatitudinalVector(1, 0, 0), new LatitudinalVector(1, 0, 0));

    assertComponentEquals(sph, (LatitudinalState) converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive X axis
   */
  @Test
  public void testXAxisState() {
    CartesianState cart = new CartesianState(VectorIJK.I, VectorIJK.I);
    LatitudinalState lat =
        new LatitudinalState(new LatitudinalVector(1, 0, 0), new LatitudinalVector(1, 0, 0));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, (LatitudinalState) converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the negative X axis
   */
  @Test
  public void testMinusXAxisState() {
    // Note, the commented out code WILL fail
    // UnwritableVectorIJK cart = VectorIJK.I.createNegated();
    CartesianState cart = new CartesianState(VectorIJK.MINUS_I, VectorIJK.MINUS_I);
    LatitudinalState lat =
        new LatitudinalState(new LatitudinalVector(1, 0, PI), new LatitudinalVector(1, 0, 0));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, (LatitudinalState) converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive Y axis
   */
  @Test
  public void testYAxisState() {
    CartesianState cart = new CartesianState(VectorIJK.J, VectorIJK.J);
    LatitudinalState lat =
        new LatitudinalState(new LatitudinalVector(1, 0, PI / 2.0), new LatitudinalVector(1, 0, 0));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, (LatitudinalState) converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the unitized vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeUnitVectorState() {
    CartesianState cart = new CartesianState(new VectorIJK(1, 1, 1).createUnitized(),
        new VectorIJK(1, 1, 1).createUnitized());
    double lati = CoordUtilities.toLatitude(Math.atan2(sqrt(2.0 / 3.0), sqrt(1.0 / 3.0)));
    LatitudinalState lat = new LatitudinalState(new LatitudinalVector(1, lati, PI / 4.0),
        new LatitudinalVector(1, 0, 0));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, (LatitudinalState) converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeVectorState() {
    CartesianState cart = new CartesianState(new VectorIJK(1, 1, 1), new VectorIJK(1, 1, 1));
    double lati = CoordUtilities.toLatitude(Math.atan2(sqrt(2.0 / 3.0), sqrt(1.0 / 3.0)));
    LatitudinalState lat = new LatitudinalState(new LatitudinalVector(sqrt(3), lati, PI / 4.0),
        new LatitudinalVector(sqrt(3), 0, 0));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, (LatitudinalState) converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing a random negative vector
   */
  @Test
  public void testNegativeVectorState() {
    CartesianState cart = new CartesianState(new VectorIJK(-1, -2, -3), new VectorIJK(-1, -2, -3));
    double lati = CoordUtilities.toLatitude(PI / 2.0 + Math.atan2(3, sqrt(5)));
    double longInRads = Math.atan2(-2, -1);
    LatitudinalState lat =
        new LatitudinalState(new LatitudinalVector(sqrt(1 + 4 + 9), lati, longInRads),
            new LatitudinalVector(3.7416573867739409, 0, 0));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, (LatitudinalState) converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing a random vector as compared directly to JPL's Spice library
   */
  @Test
  public void testWithPrecalcNumsAState() {
    CartesianState cart = new CartesianState(new VectorIJK(7, -2, 5),
        new VectorIJK(1.21342134213423, 0.21223124213421, -1.1234234687444356));
    // JPL's SPICE library gives these values for R, lati, and long:
    double r = 8.831760866327848;
    double latitude = CoordUtilities.toLatitude(0.968982551591638);
    double longitude = -0.278299659005111;
    LatitudinalState lat = new LatitudinalState(new LatitudinalVector(r, latitude, longitude),
        new LatitudinalVector(0.27767617398914923, -0.17590759831779462, 7.38200260227911131E-002));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, (LatitudinalState) converter.toCoordinate(cart), TOL);
  }

}
