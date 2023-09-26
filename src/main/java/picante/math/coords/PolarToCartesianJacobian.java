package picante.math.coords;

import static picante.math.PicanteMath.cos;
import static picante.math.PicanteMath.sin;
import picante.math.vectorspace.MatrixIJ;
import picante.math.vectorspace.UnwritableMatrixIJ;
import picante.math.vectorspace.UnwritableVectorIJ;

class PolarToCartesianJacobian implements TransformationIJ<PolarVector> {

  @Override
  public MatrixIJ getTransformation(PolarVector coordPosition, MatrixIJ buffer) {

    double r = coordPosition.getRadius();
    double angle = coordPosition.getAngle();

    double cosAngle = cos(angle);
    double sinAngle = sin(angle);

    return buffer.setTo(cosAngle, sinAngle, -r * sinAngle, r * cosAngle);
  }

  @Override
  public MatrixIJ getInverseTransformation(PolarVector coordPosition, MatrixIJ buffer) {
    try {
      return getTransformation(coordPosition, buffer).invort();
    } catch (UnsupportedOperationException e) {
      throw new PointOnAxisException(e);
    }
  }

  @Override
  public UnwritableVectorIJ mxv(UnwritableMatrixIJ jacobian, PolarVector coordVelocity) {
    return jacobian.mxv(coordVelocity.getVectorIJ());
  }

  @Override
  public PolarVector mxv(UnwritableMatrixIJ inverseJacobian, UnwritableVectorIJ cartVelocity) {
    UnwritableVectorIJ vect = inverseJacobian.mxv(cartVelocity);
    return new PolarVector(vect.getI(), vect.getJ());
  }

}
