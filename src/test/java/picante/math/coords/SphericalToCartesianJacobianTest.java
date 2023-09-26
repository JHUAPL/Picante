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
public class SphericalToCartesianJacobianTest {

  private SphericalToCartesianJacobian sph2cart;

  private final static double TOL = 1.e-15;

  private SphericalVector coordPosition;
  private SphericalVector coordVelocity;

  private double r;
  private double colat;
  private double lon;

  private double drdt;
  private double dcolatdt;
  private double dlondt;

  private double dxdtSpice;
  private double dydtSpice;
  private double dzdtSpice;

  private double drdtSpice;
  private double dcolatdtSpice;
  private double dlondtSpice;

  private VectorIJK spiceCartVelocity;

  private SphericalVector spiceSphVelocity;

  private MatrixIJK spiceJacobi;
  private MatrixIJK spiceInvJacobi;

  @Before
  public void setUp() throws Exception {
    // random
    r = 12.02243237474;
    colat = 1.257474374653;
    lon = .12323532746;

    // random
    drdt = 1.137583756;
    dcolatdt = .12385853835538;
    dlondt = .0523358358536;

    // from spice
    dxdtSpice = 1.4558992021521016;
    dydtSpice = 0.78347741519596070;
    dzdtSpice = -1.0659581934172029;

    // from spice
    drdtSpice = 1.1375837559999997;
    dcolatdtSpice = 0.12385853835537997;
    dlondtSpice = 5.23358358535999865E-002;

    spiceCartVelocity = new VectorIJK(dxdtSpice, dydtSpice, dzdtSpice);
    spiceSphVelocity = new SphericalVector(drdtSpice, dcolatdtSpice, dlondtSpice);

    coordPosition = new SphericalVector(r, colat, lon);
    coordVelocity = new SphericalVector(drdt, dcolatdt, dlondt);

    sph2cart = new SphericalToCartesianJacobian();

    spiceJacobi = new MatrixIJK(0.94410028034984717, 0.11693908997836895, 0.30822055395182651,
        3.6774582838133703, 0.45550100354084655, -11.437119375602503, -1.4058923012285767,
        11.350381775479113, 0.0000000000000000);

    spiceInvJacobi =
        new MatrixIJK(0.94410028034984717, 2.54426925031702098E-002, -1.07477794151342489E-002,
            0.11693908997836895, 3.15140813941680326E-003, 8.67715112272852418E-002,
            0.30822055395182651, -7.91283242222829419E-002, 0.0000000000000000);
  }

  @Test
  public void testGetJacobian() {

    MatrixIJK buffer = new MatrixIJK();

    assertComponentRelativeEquality(spiceJacobi, sph2cart.getTransformation(coordPosition, buffer),
        TOL);
    assertComponentRelativeEquality(spiceJacobi, buffer, TOL);

  }

  @Test
  public void testGetInverseJacobian() {

    MatrixIJK buffer = new MatrixIJK();

    assertComponentRelativeEquality(spiceJacobi.invort(),
        sph2cart.getInverseTransformation(coordPosition, buffer), TOL);
    assertComponentRelativeEquality(spiceJacobi, buffer, TOL);

    assertComponentRelativeEquality(spiceInvJacobi,
        sph2cart.getInverseTransformation(coordPosition, buffer), TOL);
    assertComponentRelativeEquality(spiceInvJacobi, buffer, TOL);
  }

  @Test
  public void testMxvUnwritableMatrixIJKUnwritableSphericalCoordVectorIJK() {

    assertComponentRelativeEquality(spiceCartVelocity,
        sph2cart.mxv(sph2cart.getTransformation(coordPosition, new MatrixIJK()), coordVelocity),
        TOL);
  }

  @Test
  public void testMxvUnwritableMatrixIJKUnwritableVectorIJKSphericalCoord() {

    assertComponentRelativeEquality(spiceSphVelocity, sph2cart.mxv(
        sph2cart.getInverseTransformation(coordPosition, new MatrixIJK()), spiceCartVelocity), TOL);
  }

}
