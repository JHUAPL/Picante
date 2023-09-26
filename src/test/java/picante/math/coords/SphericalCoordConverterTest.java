package picante.math.coords;

import static picante.math.coords.AssertTools.assertComponentEquals;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

public class SphericalCoordConverterTest {

  private static final double TOL = 1.e-14;
  private SphericalCoordConverter converter;

  @Before
  public void setUp() throws Exception {
    converter = new SphericalCoordConverter();
  }

  /**
   * Testing the positive Z axis
   */
  @Test
  public void testZAxis1() {
    UnwritableVectorIJK cart = VectorIJK.K;
    SphericalVector sph = new SphericalVector(1, 0, 0);

    assertComponentEquals(cart, converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive X axis
   */
  @Test
  public void testXAxis() {
    UnwritableVectorIJK cart = VectorIJK.I;
    SphericalVector sph = new SphericalVector(1, Math.PI / 2.0, 0);

    assertComponentEquals(cart, converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the negative X axis
   */
  @Test
  public void testMinusXAxis() {
    // Note, the commented out code WILL fail
    // UnwritableVectorIJK cart = VectorIJK.I.createNegated();
    UnwritableVectorIJK cart = VectorIJK.MINUS_I;
    SphericalVector sph = new SphericalVector(1, Math.PI / 2.0, Math.PI);

    assertComponentEquals(cart, converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive Y axis
   */
  @Test
  public void testYAxis() {
    UnwritableVectorIJK cart = VectorIJK.J;
    SphericalVector sph = new SphericalVector(1, Math.PI / 2.0, Math.PI / 2.0);

    assertComponentEquals(cart, converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the unitized vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeUnitVector() {
    UnwritableVectorIJK cart = new VectorIJK(1, 1, 1).createUnitized();
    double colat = Math.atan2(Math.sqrt(2.0 / 3.0), Math.sqrt(1.0 / 3.0));
    SphericalVector sph = new SphericalVector(1, colat, Math.PI / 4.0);

    assertComponentEquals(cart, converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeVector() {
    UnwritableVectorIJK cart = new VectorIJK(1, 1, 1);
    double colat = Math.atan2(Math.sqrt(2.0 / 3.0), Math.sqrt(1.0 / 3.0));
    SphericalVector sph = new SphericalVector(Math.sqrt(3), colat, Math.PI / 4.0);

    assertComponentEquals(cart, converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing a random negative vector
   */
  @Test
  public void testNegativeVector() {
    UnwritableVectorIJK cart = new VectorIJK(-1, -2, -3);
    double colat = Math.PI / 2.0 + Math.atan2(3, Math.sqrt(5));
    double longInRads = Math.atan2(-2, -1);
    SphericalVector sph = new SphericalVector(Math.sqrt(1 + 4 + 9), colat, longInRads);

    assertComponentEquals(cart, converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing a random vector as compared directly to JPL's Spice library
   */
  @Test
  public void testWithPrecalcNumsA() {
    UnwritableVectorIJK cart = new UnwritableVectorIJK(7, -2, 5);
    // JPL's SPICE library gives these values for R, colat, and long:
    double r = 8.831760866327848;
    double colatitude = 0.968982551591638;
    double longitude = -0.278299659005111;
    SphericalVector sph = new SphericalVector(r, colatitude, longitude);

    assertComponentEquals(cart, converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive Z axis
   */
  @Test
  public void testZAxis1State() {
    CartesianState cart = new CartesianState(VectorIJK.K, VectorIJK.K);
    SphericalState sph =
        new SphericalState(new SphericalVector(1, 0, 0), new SphericalVector(1, 0, 0));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(sph), TOL);
  }

  /**
   * Testing the positive Z axis
   */
  @Test(expected = PointOnAxisException.class)
  public void testZAxis1StateFail() {
    CartesianState cart = new CartesianState(new VectorIJK(0, 0, 5000.0143), VectorIJK.K);
    SphericalState sph =
        new SphericalState(new SphericalVector(1, 0, 0), new SphericalVector(1, 0, 0));

    assertComponentEquals(sph, (SphericalState) converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive Z axis
   */
  @Test(expected = PointOnAxisException.class)
  public void testZAxis1StateFail2() {
    CartesianState cart =
        new CartesianState(new VectorIJK(-0.0, 0.0, -6.93667171018663E11), VectorIJK.K);
    SphericalState sph =
        new SphericalState(new SphericalVector(1, 0, 0), new SphericalVector(1, 0, 0));

    assertComponentEquals(sph, (SphericalState) converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive X axis
   */
  @Test
  public void testXAxisState() {
    CartesianState cart = new CartesianState(VectorIJK.I, VectorIJK.I);
    SphericalState sph =
        new SphericalState(new SphericalVector(1, Math.PI / 2.0, 0), new SphericalVector(1, 0, 0));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, (SphericalState) converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the negative X axis
   */
  @Test
  public void testMinusXAxisState() {
    // Note, the commented out code WILL fail
    // UnwritableVectorIJK cart = VectorIJK.I.createNegated();
    CartesianState cart = new CartesianState(VectorIJK.MINUS_I, VectorIJK.MINUS_I);
    SphericalState sph = new SphericalState(new SphericalVector(1, Math.PI / 2.0, Math.PI),
        new SphericalVector(1, 0, 0));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, (SphericalState) converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive Y axis
   */
  @Test
  public void testYAxisState() {
    CartesianState cart = new CartesianState(VectorIJK.J, VectorIJK.J);
    SphericalState sph = new SphericalState(new SphericalVector(1, Math.PI / 2.0, Math.PI / 2.0),
        new SphericalVector(1, 0, 0));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, (SphericalState) converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the unitized vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeUnitVectorState() {
    CartesianState cart = new CartesianState(new VectorIJK(1, 1, 1).createUnitized(),
        new VectorIJK(1, 1, 1).createUnitized());
    double colat = Math.atan2(Math.sqrt(2.0 / 3.0), Math.sqrt(1.0 / 3.0));
    SphericalState sph = new SphericalState(new SphericalVector(1, colat, Math.PI / 4.0),
        new SphericalVector(1, 0, 0));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, (SphericalState) converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeVectorState() {
    CartesianState cart = new CartesianState(new VectorIJK(1, 1, 1), new VectorIJK(1, 1, 1));
    double colat = Math.atan2(Math.sqrt(2.0 / 3.0), Math.sqrt(1.0 / 3.0));
    SphericalState sph = new SphericalState(new SphericalVector(Math.sqrt(3), colat, Math.PI / 4.0),
        new SphericalVector(Math.sqrt(3), 0, 0));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, (SphericalState) converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing a random negative vector
   */
  @Test
  public void testNegativeVectorState() {
    CartesianState cart = new CartesianState(new VectorIJK(-1, -2, -3), new VectorIJK(-1, -2, -3));
    double colat = Math.PI / 2.0 + Math.atan2(3, Math.sqrt(5));
    double longInRads = Math.atan2(-2, -1);
    SphericalState sph =
        new SphericalState(new SphericalVector(Math.sqrt(1 + 4 + 9), colat, longInRads),
            new SphericalVector(3.7416573867739409, 0, 0));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, (SphericalState) converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing a random vector as compared directly to JPL's Spice library
   */
  @Test
  public void testWithPrecalcNumsAState() {
    CartesianState cart = new CartesianState(new VectorIJK(7, -2, 5),
        new VectorIJK(1.21342134213423, 0.21223124213421, -1.1234234687444356));
    // JPL's SPICE library gives these values for R, colat, and long:
    double r = 8.831760866327848;
    double colatitude = 0.968982551591638;
    double longitude = -0.278299659005111;
    SphericalState sph = new SphericalState(new SphericalVector(r, colatitude, longitude),
        new SphericalVector(0.27767617398914923, 0.17590759831779462, 7.38200260227911131E-002));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(sph), TOL);
    assertComponentEquals(sph, (SphericalState) converter.toCoordinate(cart), TOL);
  }

}
