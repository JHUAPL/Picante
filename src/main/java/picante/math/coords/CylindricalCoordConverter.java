package picante.math.coords;

import static picante.math.PicanteMath.abs;
import static picante.math.PicanteMath.atan2;
import static picante.math.PicanteMath.cos;
import static picante.math.PicanteMath.max;
import static picante.math.PicanteMath.sin;
import static picante.math.PicanteMath.sqrt;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.units.FundamentalPhysicalConstants;

class CylindricalCoordConverter extends AbstractCoordConverter<CylindricalVector> {

  private final static CylindricalToCartesianJacobian JACOBIAN =
      new CylindricalToCartesianJacobian();

  public CylindricalCoordConverter() {
    super(JACOBIAN);
  }

  @Override
  public CylindricalVector toCoordinate(UnwritableVectorIJK cartesian) {
    /*
     * Use temporary variables for computing R. C BIG = MAX( DABS(RECTAN(1)), DABS(RECTAN(2)) )
     * 
     * Convert to cylindrical coordinates C Z = RECTAN(3 )
     * 
     * IF ( BIG .EQ. 0 ) THEN R = 0.D0 LONG = 0.D0 ELSE X = RECTAN(1) / BIG Y = RECTAN(2) / BIG
     * 
     * R = BIG * DSQRT (X*X + Y*Y)
     * 
     * LONG = DATAN2 (Y,X) END IF
     * 
     * IF ( LONG .LT. 0.D0) THEN LONG = LONG + TWOPI() END IF
     */

    // C
    // C Use temporary variables for computing R.
    // C
    double big = max(abs(cartesian.getI()), abs(cartesian.getJ()));

    // C
    // C Convert to cylindrical coordinates
    // C
    double height = cartesian.getK();

    double cylindricalRadius = 0;
    double longitude = 0;

    if (big == 0.0) {
      cylindricalRadius = 0.0;
      longitude = 0.0;
    } else {
      double x = cartesian.getI() / big;
      double y = cartesian.getJ() / big;

      cylindricalRadius = big * sqrt(x * x + y * y);

      longitude = atan2(y, x);
    }

    if (longitude < 0.0) {
      longitude = longitude + FundamentalPhysicalConstants.TWOPI;
    }

    return new CylindricalVector(cylindricalRadius, longitude, height);
  }

  @Override
  public UnwritableVectorIJK toCartesian(CylindricalVector coordinate) {

    /*
     * From the SPICE routine cylrec.f.
     * 
     * Convert to rectangular coordinates, storing the results in C temporary variables. C X = R *
     * DCOS(LONG) Y = R * DSIN(LONG)
     * 
     * Move the results to the output variables. C RECTAN(1) = X RECTAN(2) = Y RECTAN(3) = Z
     */

    return new UnwritableVectorIJK(
        coordinate.getCylindricalRadius() * cos(coordinate.getLongitude()),
        coordinate.getCylindricalRadius() * sin(coordinate.getLongitude()), coordinate.getHeight());
  }

  @Override
  State<CylindricalVector> construct(CylindricalVector position, CylindricalVector velocity) {
    return new CylindricalState(position, velocity);
  }

}
