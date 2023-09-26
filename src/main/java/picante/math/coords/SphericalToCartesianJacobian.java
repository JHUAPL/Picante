package picante.math.coords;

import static picante.math.PicanteMath.cos;
import static picante.math.PicanteMath.sin;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.UnwritableMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;

class SphericalToCartesianJacobian implements Transformation<SphericalVector> {

  @Override
  public MatrixIJK getTransformation(SphericalVector coordPosition, MatrixIJK buffer) {
    /*
     * from SPICE's routine in drdsph.f:
     * 
     * CCOLAT = DCOS( COLAT ) SCOLAT = DSIN( COLAT )
     * 
     * CLONG = DCOS( LONG ) SLONG = DSIN( LONG )
     * 
     * 
     * JACOBI (DX,DR) = CLONG * SCOLAT
     * 
     * JACOBI (DY,DR) = SLONG * SCOLAT
     * 
     * JACOBI (DZ,DR) = CCOLAT
     * 
     * 
     * 
     * JACOBI (DX,DCOLAT) = R * CLONG * CCOLAT
     * 
     * JACOBI (DY,DCOLAT) = R * SLONG * CCOLAT
     * 
     * JACOBI (DZ,DCOLAT) = -R * SCOLAT
     * 
     * 
     * JACOBI (DX,DLON) = -R * SLONG * SCOLAT
     * 
     * JACOBI (DY,DLON) = R * CLONG * SCOLAT
     * 
     * JACOBI (DZ,DLON) = 0.0D0
     */

    double r = coordPosition.getRadius();
    double colat = coordPosition.getColatitude();
    double lon = coordPosition.getLongitude();

    double cosColat = cos(colat);
    double sinColat = sin(colat);

    double cosLong = cos(lon);
    double sinLong = sin(lon);

    double xByR = cosLong * sinColat;
    double yByR = sinLong * sinColat;
    double zByR = cosColat;

    double xByColat = r * cosLong * cosColat;
    double yByColat = r * sinLong * cosColat;
    double zByColat = -r * sinColat;

    double xByLong = -r * sinLong * sinColat;
    double yByLong = r * cosLong * sinColat;
    double zByLong = 0;

    return buffer.setTo(xByR, yByR, zByR, xByColat, yByColat, zByColat, xByLong, yByLong, zByLong);
  }

  @Override
  public MatrixIJK getInverseTransformation(SphericalVector coordPosition, MatrixIJK buffer) {
    try {
      return getTransformation(coordPosition, buffer).invort();
    } catch (UnsupportedOperationException e) {
      throw new PointOnAxisException(e);
    }
  }

  @Override
  public UnwritableVectorIJK mxv(UnwritableMatrixIJK jacobian, SphericalVector coordVelocity) {
    return jacobian.mxv(coordVelocity.getVectorIJK());
  }

  @Override
  public SphericalVector mxv(UnwritableMatrixIJK inverseJacobian,
      UnwritableVectorIJK cartVelocity) {
    UnwritableVectorIJK vect = inverseJacobian.mxv(cartVelocity);
    return new SphericalVector(vect.getI(), vect.getJ(), vect.getK());
  }

}
