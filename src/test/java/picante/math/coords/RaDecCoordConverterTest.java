package picante.math.coords;

import static java.lang.Math.PI;
import static java.lang.Math.sqrt;
import static picante.math.coords.AssertTools.assertComponentEquals;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

public class RaDecCoordConverterTest {

  private static final double TOL = 1.e-14;
  private RaDecCoordConverter converter;

  @Before
  public void setUp() throws Exception {
    converter = new RaDecCoordConverter();
  }

  /**
   * Testing the zero
   */
  @Test
  public void testZero() {
    UnwritableVectorIJK cart = VectorIJK.ZERO;
    RaDecVector lat = new RaDecVector(0, 0, 0);

    assertComponentEquals(cart, converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, converter.toCoordinate(cart), TOL);
  }


  /**
   * Testing the positive Z axis
   */
  @Test
  public void testZAxis1() {
    UnwritableVectorIJK cart = VectorIJK.K;
    RaDecVector lat = new RaDecVector(1, 0, PI / 2);

    assertComponentEquals(cart, converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive X axis
   */
  @Test
  public void testXAxis() {
    UnwritableVectorIJK cart = VectorIJK.I;
    RaDecVector lat = new RaDecVector(1, 0, 0);

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
    RaDecVector lat = new RaDecVector(1, PI, 0);

    assertComponentEquals(cart, converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, converter.toCoordinate(cart), TOL);
  }

  /**
   * Testing the positive Y axis
   */
  @Test
  public void testYAxis() {
    UnwritableVectorIJK cart = VectorIJK.J;
    RaDecVector lat = new RaDecVector(1, PI / 2.0, 0);

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
    RaDecVector sph = new RaDecVector(1, PI / 4.0, lati);

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
    RaDecVector sph = new RaDecVector(sqrt(3), PI / 4.0, lati);

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
    double longInRads = Math.atan2(-2, -1) + 2.0 * PI;
    RaDecVector lat = new RaDecVector(sqrt(1 + 4 + 9), longInRads, lati);

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
    double longitude = -0.278299659005111 + 2.0 * PI;
    RaDecVector lat = new RaDecVector(r, longitude, latitude);

    assertComponentEquals(cart, converter.toCartesian(lat), TOL);
    assertComponentEquals(lat, converter.toCoordinate(cart), TOL);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testToCartesianState() {
    converter.toCartesian(new State<RaDecVector>() {

      @Override
      public RaDecVector getPosition() {
        return new RaDecVector(0, 0, 0);
      }

      @Override
      public RaDecVector getVelocity() {
        return new RaDecVector(0, 0, 0);
      }

    });
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testToRaDecState() {
    converter.toCoordinate(new CartesianState(VectorIJK.ZERO, VectorIJK.ZERO));
  }


}
