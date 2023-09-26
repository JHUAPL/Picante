package picante.math.coords;

import static picante.math.coords.AssertTools.assertComponentRelativeEquality;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * To test, we are comparing directly to spice results.
 * 
 * @author stephgk1
 * 
 */
public class CylindricalToCartesianJacobianTest {

  private CylindricalToCartesianJacobian cyl2cart;

  private final static double TOL = 1.e-15;

  private CylindricalVector coordPosition;
  private CylindricalVector coordVelocity;

  private double r;
  private double lon;
  private double height;

  private double drdt;
  private double dlondt;
  private double dheightdt;

  private double dxdtSpice;
  private double dydtSpice;
  private double dzdtSpice;

  private double drdtSpice;
  private double dlondtSpice;
  private double dheightdtSpice;

  private VectorIJK spiceCartVelocity;

  private CylindricalVector spiceCylVelocity;

  private MatrixIJK spiceJacobi;
  private MatrixIJK spiceInvJacobi;

  @Before
  public void setUp() throws Exception {
    // random
    r = 12.02243237474;
    lon = 1.257474374653;
    height = .12323532746;

    // random
    drdt = 1.137583756;
    dlondt = .12385853835538;
    dheightdt = .0523358358536;

    // from spice
    dxdtSpice = -1.0659581934172029;
    dydtSpice = 1.5411657479813361;
    dzdtSpice = 5.23358358536000004E-2;

    // from spice
    drdtSpice = 1.1375837559999997;
    dlondtSpice = 0.12385853835537997;
    dheightdtSpice = 5.23358358535999865E-002;

    spiceCartVelocity = new VectorIJK(dxdtSpice, dydtSpice, dzdtSpice);
    spiceCylVelocity = new CylindricalVector(drdtSpice, dlondtSpice, dheightdtSpice);

    coordPosition = new CylindricalVector(r, lon, height);
    coordVelocity = new CylindricalVector(drdt, dlondt, dheightdt);

    cyl2cart = new CylindricalToCartesianJacobian();

    spiceJacobi = new MatrixIJK(0.30822055395182651, 0.95131492688889796, 0.0, -11.437119375602503,
        3.7055607663907355, 0.0, 0.0, 0.0, 1.0);

    spiceInvJacobi = new MatrixIJK(0.30822055395182651, -7.91283242222829281E-2, 0.0,
        0.95131492688889796, 2.56371210371222416E-2, 0.0, 0.0, 0.0, 1.0);
  }

  @Test
  public void testGetJacobian() {

    MatrixIJK buffer = new MatrixIJK();

    assertComponentRelativeEquality(spiceJacobi, cyl2cart.getTransformation(coordPosition, buffer),
        TOL);
    assertComponentRelativeEquality(spiceJacobi, buffer, TOL);

  }

  @Test
  public void testGetInverseJacobian() {

    MatrixIJK buffer = new MatrixIJK();

    assertComponentRelativeEquality(spiceJacobi.invort(),
        cyl2cart.getInverseTransformation(coordPosition, buffer), TOL);
    assertComponentRelativeEquality(spiceJacobi, buffer, TOL);

    assertComponentRelativeEquality(spiceInvJacobi,
        cyl2cart.getInverseTransformation(coordPosition, buffer), TOL);
    assertComponentRelativeEquality(spiceInvJacobi, buffer, TOL);
  }

  @Test
  public void testMxvUnwritableMatrixIJKUnwritablecylericalCoordVectorIJK() {

    assertComponentRelativeEquality(spiceCartVelocity,
        cyl2cart.mxv(cyl2cart.getTransformation(coordPosition, new MatrixIJK()), coordVelocity),
        TOL);
  }

  @Test
  public void testMxvUnwritableMatrixIJKUnwritableVectorIJKcylericalCoord() {

    assertComponentRelativeEquality(spiceCylVelocity, cyl2cart.mxv(
        cyl2cart.getInverseTransformation(coordPosition, new MatrixIJK()), spiceCartVelocity), TOL);

  }

}
