package picante.math.coords;

import static picante.math.PicanteMath.cos;
import static picante.math.PicanteMath.sin;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.UnwritableMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;

class SphericalToCartesianBasisTransformation implements Transformation<SphericalVector> {

  @Override
  public MatrixIJK getTransformation(SphericalVector coordPosition, MatrixIJK buffer) {

    double colat = coordPosition.getColatitude();
    double lon = coordPosition.getLongitude();

    double cosColat = cos(colat);
    double sinColat = sin(colat);

    double cosLong = cos(lon);
    double sinLong = sin(lon);

    double xByR = cosLong * sinColat;
    double yByR = sinLong * sinColat;
    double zByR = cosColat;

    double xByColat = cosLong * cosColat;
    double yByColat = sinLong * cosColat;
    double zByColat = -sinColat;

    double xByLong = -sinLong;
    double yByLong = cosLong;
    double zByLong = 0;

    return buffer.setTo(xByR, yByR, zByR, xByColat, yByColat, zByColat, xByLong, yByLong, zByLong);
  }

  @Override
  public MatrixIJK getInverseTransformation(SphericalVector coordPosition, MatrixIJK buffer) {
    // I'm pretty confident that this exception can't be thrown, the columns will always be non-zero
    // try {
    return getTransformation(coordPosition, buffer).invort();
    // } catch (UnsupportedOperationException e) {
    // throw new PointOnAxisException(e);
    // }
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
