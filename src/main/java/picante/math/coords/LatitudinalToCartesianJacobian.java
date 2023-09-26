package picante.math.coords;

import static picante.math.PicanteMath.cos;
import static picante.math.PicanteMath.sin;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.UnwritableMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;

class LatitudinalToCartesianJacobian implements Transformation<LatitudinalVector> {

  @Override
  public MatrixIJK getTransformation(LatitudinalVector coordPosition, MatrixIJK buffer) {
    /*
     * from SPICE's routine in drdlat.f:
     * 
     * JACOBI (DX,DR) = DCOS( LONG ) * DCOS( LAT )
     * 
     * JACOBI (DY,DR) = DSIN( LONG ) * DCOS( LAT )
     * 
     * JACOBI (DZ,DR) = DSIN( LAT )
     * 
     * 
     * JACOBI (DX,DLON) = -R * DSIN( LONG ) * DCOS( LAT )
     * 
     * JACOBI (DY,DLON) = R * DCOS( LONG ) * DCOS( LAT )
     * 
     * JACOBI (DZ,DLON) = 0.0D0
     * 
     * 
     * JACOBI (DX,DLAT) = -R * DCOS( LONG ) * DSIN( LAT )
     * 
     * JACOBI (DY,DLAT) = -R * DSIN( LONG ) * DSIN( LAT )
     * 
     * JACOBI (DZ,DLAT) = R * DCOS( LAT )
     */

    double r = coordPosition.getRadius();
    double lat = coordPosition.getLatitude();
    double lon = coordPosition.getLongitude();

    double cosLat = cos(lat);
    double sinLat = sin(lat);

    double cosLong = cos(lon);
    double sinLong = sin(lon);

    double xByR = cosLong * cosLat;
    double yByR = sinLong * cosLat;
    double zByR = sinLat;

    double xByLat = -r * cosLong * sinLat;
    double yByLat = -r * sinLong * sinLat;
    double zByLat = r * cosLat;

    double xByLong = -r * sinLong * cosLat;
    double yByLong = r * cosLong * cosLat;
    double zByLong = 0;
    return buffer.setTo(xByR, yByR, zByR, xByLat, yByLat, zByLat, xByLong, yByLong, zByLong);
  }

  @Override
  public MatrixIJK getInverseTransformation(LatitudinalVector coordPosition, MatrixIJK buffer) {
    try {
      return getTransformation(coordPosition, buffer).invort();
    } catch (UnsupportedOperationException e) {
      throw new PointOnAxisException(e);
    }
  }

  @Override
  public UnwritableVectorIJK mxv(UnwritableMatrixIJK jacobian, LatitudinalVector coordVelocity) {
    return jacobian.mxv(coordVelocity.getVectorIJK());
  }

  @Override
  public LatitudinalVector mxv(UnwritableMatrixIJK inverseJacobian,
      UnwritableVectorIJK cartVelocity) {
    UnwritableVectorIJK vect = inverseJacobian.mxv(cartVelocity);
    return new LatitudinalVector(vect.getI(), vect.getJ(), vect.getK());
  }

}
