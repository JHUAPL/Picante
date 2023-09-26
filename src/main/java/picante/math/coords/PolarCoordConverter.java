package picante.math.coords;

import static picante.math.PicanteMath.abs;
import static picante.math.PicanteMath.atan2;
import static picante.math.PicanteMath.cos;
import static picante.math.PicanteMath.max;
import static picante.math.PicanteMath.sin;
import static picante.math.PicanteMath.sqrt;
import picante.math.vectorspace.UnwritableVectorIJ;

class PolarCoordConverter extends AbstractCoordConverterIJ<PolarVector> {

  private final static PolarToCartesianJacobian JACOBIAN = new PolarToCartesianJacobian();

  public PolarCoordConverter() {
    super(JACOBIAN);
  }

  @Override
  public PolarVector toCoordinate(UnwritableVectorIJ cartesian) {

    /*
     * From the SPICE routine recsph.f, here is an algorithm for converting to polar polar
     * coordinates from rectangular coordiantes: C C Store rectangular coordinates in temporary
     * variables C BIG = MAX ( DABS(RECTAN(1)), DABS(RECTAN(2)), DABS(RECTAN(3)) )
     * 
     * IF ( BIG .GT. 0 ) THEN
     * 
     * X = RECTAN(1) / BIG Y = RECTAN(2) / BIG Z = RECTAN(3) / BIG
     * 
     * R = BIG * DSQRT (X*X + Y*Y + Z*Z)
     * 
     * COLAT = DATAN2 ( DSQRT(X*X + Y*Y), Z )
     * 
     * X = RECTAN(1) Y = RECTAN(2)
     * 
     * IF (X.EQ.0.0D0 .AND. Y.EQ.0.0D0) THEN LONG = 0.0D0 ELSE LONG = DATAN2 (Y,X) END IF
     * 
     * ELSE R = 0.0D0 COLAT = 0.0D0 LONG = 0.0D0 END IF
     * 
     * RETURN END
     */
    double x = cartesian.getI();
    double y = cartesian.getJ();

    double radius = 0;
    double angle = 0;

    double big = max(abs(x), abs(y));
    if (big > 0) {
      x /= big;
      y /= big;

      radius = big * sqrt(x * x + y * y);
      angle = atan2(y, x);

      x = cartesian.getI();
      y = cartesian.getJ();

    } else {
      radius = 0;
      angle = 0;
    }

    return new PolarVector(radius, angle);
  }

  @Override
  public UnwritableVectorIJ toCartesian(PolarVector coordinate) {
    /*
     * from the SPICE routine sphrec.f, here is a formula for converting from polar polar
     * coordinates to rectangular coordinates: X = R * DCOS(LONG) * DSIN(COLAT) Y = R * DSIN(LONG) *
     * DSIN(COLAT) Z = R * DCOS(COLAT)
     */
    double i = coordinate.getRadius() * cos(coordinate.getAngle());
    double j = coordinate.getRadius() * sin(coordinate.getAngle());

    return new UnwritableVectorIJ(i, j);
  }

  @Override
  PolarState construct(PolarVector position, PolarVector velocity) {
    return new PolarState(position, velocity);
  }

}
