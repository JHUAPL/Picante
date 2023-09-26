package picante.math.coords;

import static picante.math.PicanteMath.abs;
import static picante.math.PicanteMath.atan2;
import static picante.math.PicanteMath.cos;
import static picante.math.PicanteMath.max;
import static picante.math.PicanteMath.sin;
import static picante.math.PicanteMath.sqrt;
import picante.math.vectorspace.UnwritableVectorIJK;

class SphericalCoordConverter extends AbstractCoordConverter<SphericalVector> {

  private final static SphericalToCartesianJacobian JACOBIAN = new SphericalToCartesianJacobian();

  public SphericalCoordConverter() {
    super(JACOBIAN);
  }

  @Override
  public SphericalVector toCoordinate(UnwritableVectorIJK cartesian) {

    /*
     * From the SPICE routine recsph.f, here is an algorithm for converting to spherical polar
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
    double z = cartesian.getK();

    double radius = 0;
    double colatitude = 0;
    double longitude = 0;

    double big = max(max(abs(x), abs(y)), abs(z));
    if (big > 0) {
      x /= big;
      y /= big;
      z /= big;

      radius = big * sqrt(x * x + y * y + z * z);
      colatitude = atan2(sqrt(x * x + y * y), z);

      x = cartesian.getI();
      y = cartesian.getJ();

      if ((x == 0) && (y == 0)) {
        longitude = 0;
      } else {
        longitude = atan2(y, x);
      }
    } else {
      radius = 0;
      colatitude = 0;
      longitude = 0;
    }

    return new SphericalVector(radius, colatitude, longitude);
  }

  @Override
  public UnwritableVectorIJK toCartesian(SphericalVector coordinate) {
    /*
     * from the SPICE routine sphrec.f, here is a formula for converting from spherical polar
     * coordinates to rectangular coordinates:
     * 
     * X = R * DCOS(LONG) * DSIN(COLAT)
     * 
     * Y = R * DSIN(LONG) * DSIN(COLAT)
     * 
     * Z = R * DCOS(COLAT)
     */
    double r = coordinate.getRadius();

    double cosLong = cos(coordinate.getLongitude());
    double sinLong = sin(coordinate.getLongitude());

    double cosColat = cos(coordinate.getColatitude());
    double sinColat = sin(coordinate.getColatitude());

    double i = r * cosLong * sinColat;
    double j = r * sinLong * sinColat;
    double k = r * cosColat;

    return new UnwritableVectorIJK(i, j, k);
  }

  @Override
  State<SphericalVector> construct(SphericalVector position, SphericalVector velocity) {
    return new SphericalState(position, velocity);
  }

  /**
   * This was necessary, it is not always a number close enough to zero to get the make the invort
   * method throw this exception
   */
  @Override
  public State<SphericalVector> toCoordinate(State<UnwritableVectorIJK> cartesian) {
    if (cartesian.getPosition().getI() == 0.0 && cartesian.getPosition().getJ() == 0.0) {
      throw new PointOnAxisException();
    }
    return super.toCoordinate(cartesian);
  }

}
