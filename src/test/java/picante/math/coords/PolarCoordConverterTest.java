package picante.math.coords;

import static java.lang.Math.sqrt;
import static picante.math.coords.AssertTools.assertComponentEquals;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.UnwritableVectorIJ;
import picante.math.vectorspace.VectorIJ;

public class PolarCoordConverterTest {

  private PolarCoordConverter polarCoordConverter;

  private static final double TOL = 1.0E-13;

  @Before
  public void setUp() throws Exception {
    this.polarCoordConverter = new PolarCoordConverter();
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

    assertComponentEquals(polar1, polarCoordConverter.toCoordinate(vector1), 0.0);
    assertComponentEquals(polar2, polarCoordConverter.toCoordinate(vector2), 0.0);
    assertComponentEquals(polar3, polarCoordConverter.toCoordinate(vector3), 0.0);
    assertComponentEquals(polar4, polarCoordConverter.toCoordinate(vector4), 0.0);
    assertComponentEquals(polar5, polarCoordConverter.toCoordinate(vector5), 0.0);
    assertComponentEquals(polar6, polarCoordConverter.toCoordinate(vector6), 0.0);
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

    assertComponentEquals(vector1, polarCoordConverter.toCartesian(polar1), 0.0);
    assertComponentEquals(vector2, polarCoordConverter.toCartesian(polar2), TOL);
    assertComponentEquals(vector3, polarCoordConverter.toCartesian(polar3), TOL);
    assertComponentEquals(vector4, polarCoordConverter.toCartesian(polar4), TOL);
    assertComponentEquals(vector5, polarCoordConverter.toCartesian(polar5), TOL);
    assertComponentEquals(vector6, polarCoordConverter.toCartesian(polar6), TOL);
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

    assertComponentEquals(polar1, (PolarState) polarCoordConverter.toCoordinate(state1), TOL);
    assertComponentEquals(polar2, (PolarState) polarCoordConverter.toCoordinate(state2), TOL);

  }

  @Test(expected = PointOnAxisException.class)
  public void testToCoordinateStateOfUnwritableVectorIJException() {

    CartesianStateIJ state1 =
        new CartesianStateIJ(new UnwritableVectorIJ(0, 0), new UnwritableVectorIJ(3, -.5));

    polarCoordConverter.toCoordinate(state1);
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

    assertComponentEquals(state1, (CartesianStateIJ) polarCoordConverter.toCartesian(polar1), TOL);
    assertComponentEquals(state2, (CartesianStateIJ) polarCoordConverter.toCartesian(polar2), TOL);
  }

  private final static double getdrdt(double x, double y, double dxdt, double dydt) {
    return (x * dxdt + y * dydt) / sqrt(x * x + y * y);
  }

  private final static double getdthetadt(double x, double y, double dxdt, double dydt) {
    return (x * dydt - y * dxdt) / (x * x + y * y);
  }

}
