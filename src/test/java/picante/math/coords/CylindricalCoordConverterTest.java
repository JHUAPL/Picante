package picante.math.coords;

import static java.lang.Math.PI;
import static java.lang.Math.atan2;
import static java.lang.Math.sqrt;
import static org.junit.Assert.assertSame;
import static picante.math.coords.AssertTools.assertComponentEquals;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.units.FundamentalPhysicalConstants;

public class CylindricalCoordConverterTest {

  private static final double TOL = 1.e-14;
  private CylindricalCoordConverter converter;

  @Before
  public void setUp() throws Exception {
    converter = new CylindricalCoordConverter();
  }

  /**
   * Testing the positive Z axis
   */
  @Test
  public void testZAxis1() {
    UnwritableVectorIJK cart = VectorIJK.K;
    CylindricalVector cyl = new CylindricalVector(0, 0, 1);

    assertComponentEquals(cart, converter.toCartesian(cyl), TOL);
    assertComponentEquals(cyl, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive X axis
   */
  @Test
  public void testXAxis() {
    UnwritableVectorIJK cart = VectorIJK.I;
    CylindricalVector cyl = new CylindricalVector(1, 0, 0);

    assertComponentEquals(cart, converter.toCartesian(cyl), TOL);
    assertComponentEquals(cyl, converter.toCoordinate(cart), TOL);
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

    assertComponentEquals(cart, converter.toCartesian(cyl), TOL);
    assertComponentEquals(cyl, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive Y axis
   */
  @Test
  public void testYAxis() {
    UnwritableVectorIJK cart = VectorIJK.J;
    CylindricalVector cyl = new CylindricalVector(1, PI / 2.0, 0);

    assertComponentEquals(cart, converter.toCartesian(cyl), TOL);
    assertComponentEquals(cyl, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the negative Y axis
   */
  @Test
  public void testMinusYAxis() {
    UnwritableVectorIJK cart = VectorIJK.MINUS_J;
    CylindricalVector cyl = new CylindricalVector(1, 3.0 * PI / 2.0, 0);

    assertComponentEquals(cart, converter.toCartesian(cyl), TOL);
    assertComponentEquals(cyl, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the unitized vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeUnitVector() {
    UnwritableVectorIJK cart = new VectorIJK(1, 1, 1).createUnitized();
    CylindricalVector cyl = new CylindricalVector(sqrt(2.0 / 3.0), PI / 4.0, 1.0 / sqrt(3.0));

    assertComponentEquals(cart, converter.toCartesian(cyl), TOL);
    assertComponentEquals(cyl, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the vector pointing to 1, 1, 1 in Cartesian
   */
  @Test
  public void test45DegreeVector() {
    UnwritableVectorIJK cart = new VectorIJK(1, 1, 1);
    CylindricalVector cyl = new CylindricalVector(sqrt(2.0), PI / 4.0, 1.0);

    assertComponentEquals(cart, converter.toCartesian(cyl), TOL);
    assertComponentEquals(cyl, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing a random negative vector
   */
  @Test
  public void testNegativeVector() {
    UnwritableVectorIJK cart = new VectorIJK(-1, -2, -3);
    CylindricalVector cyl =
        new CylindricalVector(sqrt(1 + 4), atan2(-2, -1) + FundamentalPhysicalConstants.TWOPI, -3);

    assertComponentEquals(cart, converter.toCartesian(cyl), TOL);
    assertComponentEquals(cyl, converter.toCoordinate(cart), TOL);
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

    assertComponentEquals(cart, converter.toCartesian(cyl), TOL);
    assertComponentEquals(cyl, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive Z axis
   */
  @Test
  public void testZAxis1State() {
    CartesianState cart = new CartesianState(VectorIJK.K, VectorIJK.K);
    CylindricalState cyl =
        new CylindricalState(new CylindricalVector(0, 0, 1), new CylindricalVector(0, 0, 1));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(cyl), TOL);
  }

  /**
   * Testing the positive Z axis
   */
  @Test(expected = PointOnAxisException.class)
  public void testZAxis1StateFail() {
    CartesianState cart = new CartesianState(VectorIJK.K, VectorIJK.K);
    CylindricalState cyl =
        new CylindricalState(new CylindricalVector(0, 0, 1), new CylindricalVector(0, 0, 1));

    CylindricalState cylBuffer = CylindricalState.ZERO;

    assertComponentEquals(cyl, (CylindricalState) converter.toCoordinate(cart), TOL);
    assertComponentEquals(cyl, cylBuffer, TOL);
    assertSame(cylBuffer, converter.toCoordinate(cart));
  }

  /**
   * Testing the positive X axis
   */
  @Test
  public void testXAxisState() {
    CartesianState cart = new CartesianState(VectorIJK.I, VectorIJK.I);
    CylindricalState cyl =
        new CylindricalState(new CylindricalVector(1, 0, 0), new CylindricalVector(1, 0, 0));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(cyl), TOL);
    assertComponentEquals(cyl, (CylindricalState) converter.toCoordinate(cart), TOL);
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

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(cyl), TOL);
    assertComponentEquals(cyl, (CylindricalState) converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive Y axis
   */
  @Test
  public void testYAxisState() {
    CartesianState cart = new CartesianState(VectorIJK.J, VectorIJK.J);
    CylindricalState cyl =
        new CylindricalState(new CylindricalVector(1, PI / 2.0, 0), new CylindricalVector(1, 0, 0));

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(cyl), TOL);
    assertComponentEquals(cyl, (CylindricalState) converter.toCoordinate(cart), TOL);
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

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(cyl), TOL);
    assertComponentEquals(cyl, (CylindricalState) converter.toCoordinate(cart), TOL);
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

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(cyl), TOL);
    assertComponentEquals(cyl, (CylindricalState) converter.toCoordinate(cart), TOL);
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

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(cyl), TOL);
    assertComponentEquals(cyl, (CylindricalState) converter.toCoordinate(cart), TOL);
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

    assertComponentEquals(cart, (CartesianState) converter.toCartesian(cyl), TOL);
    assertComponentEquals(cyl, (CylindricalState) converter.toCoordinate(cart), TOL);
  }

}
