package picante.math.coords;

import static picante.math.PicanteMath.abs;
import static picante.math.PicanteMath.atan2;
import static picante.math.PicanteMath.cos;
import static picante.math.PicanteMath.max;
import static picante.math.PicanteMath.sin;
import static picante.math.PicanteMath.sqrt;
import picante.math.vectorspace.UnwritableVectorIJK;

class LatitudinalCoordConverter extends AbstractCoordConverter<LatitudinalVector> {

  private final static LatitudinalToCartesianJacobian JACOBIAN =
      new LatitudinalToCartesianJacobian();

  public LatitudinalCoordConverter() {
    super(JACOBIAN);
  }

  @Override
  public LatitudinalVector toCoordinate(UnwritableVectorIJK cartesian) {
    /*
     * From the SPICE routine reclat.f.
     * 
     * BIG = MAX ( DABS(RECTAN(1)), DABS(RECTAN(2)), DABS(RECTAN(3)) )
     * 
     * IF ( BIG .GT. 0 ) THEN
     * 
     * X = RECTAN(1) / BIG Y = RECTAN(2) / BIG Z = RECTAN(3) / BIG
     * 
     * RADIUS = BIG * DSQRT (X*X + Y*Y + Z*Z)
     * 
     * LAT = DATAN2 ( Z, DSQRT(X*X + Y*Y) )
     * 
     * X = RECTAN(1) Y = RECTAN(2)
     * 
     * IF (X.EQ.0.D0 .AND. Y.EQ.0.D0) THEN LONG = 0.D0 ELSE LONG = DATAN2 (Y,X) END IF
     * 
     * ELSE RADIUS = 0.0D0 LAT = 0.D0 LONG = 0.D0 END IF
     */

    double x = cartesian.getI();
    double y = cartesian.getJ();
    double z = cartesian.getK();

    double radius = 0;
    double latitude = 0;
    double longitude = 0;

    double big = max(max(abs(x), abs(y)), abs(z));
    if (big > 0) {
      x /= big;
      y /= big;
      z /= big;

      radius = big * sqrt(x * x + y * y + z * z);
      latitude = atan2(z, sqrt(x * x + y * y));

      x = cartesian.getI();
      y = cartesian.getJ();

      if ((x == 0) && (y == 0)) {
        longitude = 0;
      } else {
        longitude = atan2(y, x);
      }
    } else {
      radius = 0;
      latitude = 0;
      longitude = 0;
    }

    return new LatitudinalVector(radius, latitude, longitude);
  }

  @Override
  public UnwritableVectorIJK toCartesian(LatitudinalVector coordinate) {

    /*
     * From the SPICE routine latrec.f.
     * 
     * X = RADIUS * DCOS(LONG) * DCOS(LAT)
     * 
     * Y = RADIUS * DSIN(LONG) * DCOS(LAT)
     * 
     * Z = RADIUS * DSIN(LAT)
     */

    double i =
        coordinate.getRadius() * cos(coordinate.getLongitude()) * cos(coordinate.getLatitude());
    double j =
        coordinate.getRadius() * sin(coordinate.getLongitude()) * cos(coordinate.getLatitude());
    double k = coordinate.getRadius() * sin(coordinate.getLatitude());

    return new UnwritableVectorIJK(i, j, k);
  }

  @Override
  State<LatitudinalVector> construct(LatitudinalVector position, LatitudinalVector velocity) {
    return new LatitudinalState(position, velocity);
  }

  /**
   * This was necessary, Math.cos(Math.PI/2.0) does not give a number close enough to zero to get
   * the make the invort method throw this exception, this is not how this is handled in the
   * Spherical case
   */
  @Override
  public State<LatitudinalVector> toCoordinate(State<UnwritableVectorIJK> cartesian) {
    if (cartesian.getPosition().getI() == 0.0 && cartesian.getPosition().getJ() == 0.0) {
      throw new PointOnAxisException();
    }
    return super.toCoordinate(cartesian);
  }

}
